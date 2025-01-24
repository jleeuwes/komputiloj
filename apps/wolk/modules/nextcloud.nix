{ boltons, nextcloud, nixpkgsCurrent, users, ... }:
with boltons;
{ ... }:
let
    escapeShellArg = nixpkgsCurrent.lib.strings.escapeShellArg;
    bigstorageService = rec {
        serviceConfig = {
            Type = "simple";
            User = "nextcloud";
            PermissionsStartOnly = true;
        };
        needsStorageVolume = "requires";
        wants = [
            "bigstorage1-wolk-webdav-pass-key.service"
            "bigstorage1-wolk-crypt-pass-key.service"
            "bigstorage1-wolk-crypt-salt-key.service"
        ];
        after = wants;
        mailOnFailure = true;
        path = [
            nixpkgsCurrent.packages.rclone
            nixpkgsCurrent.packages.gnugrep
            nixpkgsCurrent.packages.coreutils
            nixpkgsCurrent.packages.utillinux
        ];
        preStart = ''
            # This script should run as root! Check PermissionsStartOnly.
            
            # To work around https://forum.rclone.org/t/detected-overridden-config/23371 and https://forum.rclone.org/t/different-cache-dir-suffixes-for-same-password/41114
            # we generate a config file instead of using environment variables.
            # This ExecPreStart runs as root so we can put the config on ramfs /run/keys
            config_file=/run/keys/bigstorage1-wolk-config
            umask u=rw,go=
            touch -- "$config_file"
            chown root:nextcloud -- "$config_file"
            chmod g+r -- "$config_file"
            cat > "$config_file" <<-EOF
                [lower]
                type=webdav
                url=https://u362967-sub1.your-storagebox.de
                vendor=other
                user=u362967-sub1
                pass=$(rclone obscure - < /run/keys/bigstorage1-wolk-webdav-pass)

                [upper]
                type=crypt
                remote=lower:encrypted
                password=$(rclone obscure - < /run/keys/bigstorage1-wolk-crypt-pass)
                password2=$(rclone obscure - < /run/keys/bigstorage1-wolk-crypt-salt)
            EOF
        '';
        script = ''
            # ExecStart (this script) MUST NOT RUN AS ROOT!

            # Give rclone access to fusermount3 wrapper
            # (which hopefully is installed by default)
            export PATH="$PATH":/run/wrappers/bin
            
            config_file=/run/keys/bigstorage1-wolk-config
            
            # NOTE: the cache dir should not be deleted without thought:
            # it can contain yet-to-be-written files.
            # So TODO maybe we should put it on /mnt/storage/live instead of /mnt/storage/work.
            # (Allthough we back up with far bigger delay than the write cache delay,
            # so it won't necessarily reduce the amount of lost data in case of losing our storage volume.)
            rclone mount upper: /mnt/per-user/nextcloud/bigstorage \
                --config="$config_file" \
                --vfs-cache-mode=writes \
                --vfs-cache-max-size=1G \
                --cache-dir=/mnt/storage/work/nextcloud/vfs-cache
        '';
        postStart = ''
            function is_mounted() {
                # ExecStartPost runs as root, but root cannot inspect nextcloud's fuse mount,
                # so we need to drop priviliges:
                setpriv --reuid=nextcloud --regid=nextcloud --clear-groups mountpoint "$@"
            }
            
            while kill -0 "$MAINPID"; do
                if is_mounted /mnt/per-user/nextcloud/bigstorage; then
                    echo "Bigstorage mounted."
                    break
                else
                    echo "Waiting for bigstorage to mount..."
                    sleep 1
                fi
            done
        '';
        postStop = ''
            export PATH="$PATH":/run/wrappers/bin
            fusermount -u /mnt/per-user/nextcloud/bigstorage
        '';
    };
    bindmountService = user: rec {
        serviceConfig = {
            Type = "simple";
            User = "nextcloud";
        };
        needsStorageVolume = "requires";
        requires = [
            "mount-nextcloud-bigstorage.service"
        ];
        after = requires;
        mailOnFailure = true;
        path = [
            nixpkgsCurrent.packages.bindfs
            nixpkgsCurrent.packages.coreutils
            nixpkgsCurrent.packages.utillinux
        ];
        preStart = ''
        '';
        script = ''
            export PATH="$PATH":/run/wrappers/bin
            user=${escapeShellArg user}
            userdir=/mnt/storage/live/nextcloud/rootdir/data/"$user"
            bigdir=/mnt/per-user/nextcloud/bigstorage/"$user"

            function is_empty_dir() {
                [[ -z "$(ls -A1q -- "$1")" ]]
            }

            if [[ ! -d "$userdir" ]]; then
                printf '<3>Data dir %q does not exist so we cannot mount there.\n' "$userdir"
                exit 1
            fi
            mkdir -p  -- "$userdir"/files
            if ! is_empty_dir "$userdir"/files; then
                if is_empty_dir "$bigdir"; then
                    printf '<3>There are files in %q that would disappear behind the bindmount. Aborting.\n' "$userdir"/files
                    exit 1
                else
                    # The <4> makes it a warning: https://wiki.archlinux.org/title/Systemd/Journal
                    printf '<4>There are files in both %q and %q! I will continue but this should be investigated.\n' "$userdir"/files "$bigdir"
                fi
            fi
            
            # Dir must be writable to be able to mount
            chmod u+w -- "$userdir"/files
            # Slight window between chmod and bindfs in which files might be written :(
            # Option -f makes bindfs run in the foreground
            bindfs -f --no-allow-other "$bigdir" "$userdir"/files
        '';
        postStart = ''
            user=${escapeShellArg user}
            while kill -0 "$MAINPID"; do
                if mountpoint -q /mnt/storage/live/nextcloud/rootdir/data/"$user"/files; then
                    echo "Nextcloud user bindmounted: $user"
                    break
                else
                    echo "Waiting for nextcloud user to mount: $user"
                    sleep 1
                fi
            done
        '';
        postStop = ''
            export PATH="$PATH":/run/wrappers/bin
            user=${escapeShellArg user}
            userdir=/mnt/storage/live/nextcloud/rootdir/data/"$user"

            fusermount -u -- "$userdir"/files
            # Try and prevent writing to the mountpoint when not mounted:
            chmod ug-w -- "$userdir"/files
            
            function is_empty_dir() {
                [[ -z "$(ls -A1q -- "$1")" ]]
            }

            if ! is_empty_dir "$userdir"/files; then
                printf '<3>There are files in %q that would disappear behind the bindmount next time.\n' "$userdir"/files
                exit 1 # attempt to trigger failure so we get a mail
            fi
        '';
    };
    userlist = attrValues users;
    usersWithBigstorage = filter (user: user.apps.wolk.bigstorage or false) userlist;
    bindmountServices = listToAttrs (map (user: {
        name = "mount-nextcloud-bindmount-${user.name}";
        value = bindmountService user.name;
    }) usersWithBigstorage);
    bindmountServicesAsDeps = map (nm: "${nm}.service") (attrNames bindmountServices);
in {
    imports = [
        # TODO depends on storage-volume
    ];

    options = {
    };

    config = {
        users = {
            users.nextcloud = {
                uid = 70000;
                group = "nextcloud";
                extraGroups = [ "keys" ];
            };
            groups.nextcloud = {
                gid = 70000;
            };
        };
        services.nginx = {
            virtualHosts = {
                "wolk.radstand.nl" = {
                    forceSSL = true;
                    enableACME = true;
                };
            };
        };
        
        services.nextcloud = {
            enable = true;

            package = nextcloud.packages.nextcloud;

            home = "/mnt/storage/live/nextcloud/rootdir";

            autoUpdateApps = {
                enable = true;
            };

            hostName = "wolk.radstand.nl";
            https = true; # no idea how this relates to config.overwriteProtocol

            maxUploadSize = "512M";

            config = {
                adminuser = "admin";
                adminpassFile = "/run/keys/nextcloud-admin";
                
                dbtype = "sqlite"; # let's start simple
            };

            settings = {
                overwriteprotocol = "https";

                default_phone_region = "NL";

                # Tame image previews:
                preview_max_x = 1024;
                preview_max_y = 1024;
                preview_max_filesize_image = 5;
                
                # Important for making git-annex shared repos work
                "localstorage.umask" = "002";
                "lost_password_link" = "https://sleutel.radstand.nl/wachtwoord-vergeten";
            };

            extraApps = {
                inherit (nextcloud.packages.apps) files_linkeditor calendar;
            };
        };

        systemd.services = bindmountServices // {
            mount-nextcloud-bigstorage = bigstorageService;

            # Augment nextcloud's own services:
            # TODO wants nextcloud-admin key (setup only?)
            nextcloud-cron = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                wants = bindmountServicesAsDeps;
                after = bindmountServicesAsDeps;
            };
            nextcloud-setup = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
            };
            nextcloud-update-plugins = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
            };
            phpfpm-nextcloud = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                wants = bindmountServicesAsDeps;
                after = bindmountServicesAsDeps;
            };
        };
    };
}
