{ boltons, nextcloud, nixpkgsCurrent, ... }:
with boltons;
{ ... }:
let
    # TODO pull lib out of packages into the capsule?
    escapeShellArg = nixpkgsCurrent.packages.lib.strings.escapeShellArg;
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
            user=${escapeShellArg user}
            userdir=/mnt/storage/live/nextcloud/rootdir/data/"$user"
            bigdir=/mnt/per-user/nextcloud/bigstorage/"$user"

            function is_empty_dir() {
                [[ -z "$(ls -A1q -- "$1")" ]]
            }

            if [[ ! -d "$userdir" ]]; then
                printf '<3>Data dir %q does not exist so we can't mount there.\n' "$userdir"
                exit 1
            fi
            mkdir -p  -- "$userdir"/files
            if ! is_empty_dir "$userdir"/files)"; then
                if is_empty_dir "$bigdir"; then
                    printf '<3>There are files in %q that would disappear behind the bindmount. Aborting.\n' "$userdir"/files
                    exit 1
                else
                    # The <4> makes it a warning: https://wiki.archlinux.org/title/Systemd/Journal
                    printf '<4>There are files in both %q and %q! I'll continue but this should be investigated.\n' "$userdir"/files "$bigdir"
                fi
            fi
            
            # Dir must be writable to be able to mount
            chmod u+w -- "$userdir"/files
            # Slight window between chmod and bindfs in which files might be written :(
            # Option -f makes bindfs run in the foreground
            bindfs -f --no-allow-other -- "$bigdir" "$userdir"/files
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
            user=${escapeShellArg user}
            userdir=/mnt/storage/live/nextcloud/rootdir/data/"$user"
            export PATH="$PATH":/run/wrappers/bin

            fusermount -u -- "$userdir"/files
            # Try and prevent writing to the mountpoint when not mounted:
            chmod ug-w -- "$userdir"/files
            
            function is_empty_dir() {
                [[ -z "$(ls -A1q -- "$1")" ]]
            }

            if ! is_empty_dir "$userdir"/files)"; then
                printf '<3>There are files in %q that would disappear behind the bindmount next time.\n' "$userdir"/files
                exit 1 # attempt to trigger failure so we get a mail
            fi
        '';
    };
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

            enableBrokenCiphersForSSE = false; # https://github.com/NixOS/nixpkgs/pull/198470

            config = {
                adminuser = "admin";
                adminpassFile = "/run/keys/nextcloud-admin";
                
                dbtype = "sqlite"; # let's start simple
                
                overwriteProtocol = "https";
            };

            extraApps = {
                inherit (nextcloud.packages.apps) files_linkeditor calendar;
            };
        };

        systemd.services = {
            mount-nextcloud-bigstorage = bigstorageService;

            mount-nextcloud-bindmount-testje = bindmountService "testje";
            mount-nextcloud-bindmount-jeroen = bindmountService "jeroen";

            # Augment nextcloud's own services:
            # TODO requisite nextcloud-admin key (setup only?)
            nextcloud-cron = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                # requires = [
                #     "mount-nextcloud-bindmounts.service"
                # ];
                # after = requires;
            };
            nextcloud-setup = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                # requires = [
                #     "mount-nextcloud-bindmounts.service"
                # ];
                # after = requires;
            };
            nextcloud-update-plugins = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                # requires = [
                #     "mount-nextcloud-bindmounts.service"
                # ];
                # after = requires;
            };
            phpfpm-nextcloud = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                # requires = [
                #     "mount-nextcloud-bindmounts.service"
                # ];
                # after = requires;
            };
        };
    };
}
