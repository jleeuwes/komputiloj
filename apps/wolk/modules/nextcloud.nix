{ boltons, nextcloud, nixpkgsCurrent, ... }:
{ ... }:
{
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
            mount-nextcloud-bigstorage = rec {
                serviceConfig = {
                    Type = "simple";
                    User = "nextcloud";
                };
                needsStorageVolume = "requires";
                requires = [
                    "bigstorage1-wolk-webdav-pass-key.service"
                    "bigstorage1-wolk-crypt-pass-key.service"
                    "bigstorage1-wolk-crypt-salt-key.service"
                ];
                after = requires;
                mailOnFailure = true;
                path = [
                    nixpkgsCurrent.packages.rclone
                    nixpkgsCurrent.packages.gnugrep
                    nixpkgsCurrent.packages.coreutils
                    nixpkgsCurrent.packages.utillinux
                ];
                script = ''
                    # Give rclone access to fusermount3 wrapper
                    # (which hopefully is installed by default)
                    export PATH="$PATH":/run/wrappers/bin

                    export RCLONE_CONFIG_LOWER_TYPE=webdav
                    export RCLONE_CONFIG_LOWER_URL=https://u362967-sub1.your-storagebox.de
                    export RCLONE_CONFIG_LOWER_VENDOR=other
                    export RCLONE_CONFIG_LOWER_USER=u362967-sub1
                    export RCLONE_CONFIG_LOWER_PASS=$(rclone obscure - < /run/keys/bigstorage1-wolk-webdav-pass)
                    export RCLONE_CONFIG_UPPER_TYPE=crypt
                    export RCLONE_CONFIG_UPPER_REMOTE=lower:encrypted
                    export RCLONE_CONFIG_UPPER_PASSWORD=$(rclone obscure - < /run/keys/bigstorage1-wolk-crypt-pass)
                    export RCLONE_CONFIG_UPPER_PASSWORD2=$(rclone obscure - < /run/keys/bigstorage1-wolk-crypt-salt)
                    rclone mount upper: /mnt/per-user/nextcloud/bigstorage \
                        --config=/dev/null \
                        --vfs-cache-mode=writes \
                        --vfs-cache-max-size=1G \
                        --cache-dir=/mnt/storage/work/nextcloud/vfs-cache
                '';
                postStart = ''
                    while kill -0 "$MAINPID"; do
                        if mountpoint -q /mnt/per-user/nextcloud/bigstorage; then
                            echo "Bigstorage mounted."
                            break
                        else
                            echo "Waiting for bigstorage to mount..."
                            sleep 1
                        fi
                    done
                '';
                preStop = ''
                    export PATH="$PATH":/run/wrappers/bin
                    fusermount -u /mnt/per-user/nextcloud/bigstorage
                '';
            };

            mount-nextcloud-bindmounts = rec {
                serviceConfig = {
                    Type = "oneshot";
                    User = "nextcloud";
                };
                needsStorageVolume = "requires";
                requires = [
                    "mount-nextcloud-bigstorage.service"
                ];
                after = requires;
                mailOnFailure = true;
                path = [
                ];
                script = ''
                    echo PLACEHOLDER
                '';
            };

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
