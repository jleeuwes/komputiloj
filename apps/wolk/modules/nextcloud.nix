{ boltons, nextcloud, ... }:
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
            # Augment nextcloud's own services:
            # TODO requisite nextcloud-admin key (setup only?)
            nextcloud-cron = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
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
            };
        };
    };
}
