{ boltons, nextcloud, nixpkgsCurrent, all, ... }: {
    modules = rec {
        all_in_one = {
            imports = [
                silverbullet
                # users # problematic when service is not enabled
                nginx
            ];
        };
        silverbullet = {pkgs, ...}: let
            envFile = pkgs.writeText "silverbullet.env" ''
                SB_KV_DB=/mnt/storage/live/silverbullet/spaces/notie.sqlite
            '';
        in {
            services.silverbullet = {
                enable = false; # DO NOT ENABLE WITHOUT SETTING A BASICAUTH PASSWORD BELOW
                # currently only one space is supported
                spaceDir = "/mnt/storage/live/silverbullet/spaces/notie";
                envFile = "${envFile}";
                listenPort = 3010;
            };
            systemd.services.silverbullet = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
            };
        };
        users = {
            users.users.silverbullet.uid = 70006;
            users.groups.silverbullet.gid = 70006;
        };
        nginx = {
            services.nginx.virtualHosts."notie.radstand.nl" = {
                forceSSL = true;
                enableACME = true;
                basicAuth = {
                    user = "TODO-SET-PASSWORD";
                };
                locations = {
                    "/" = {
                        proxyPass = "http://localhost:3010/";
                        # extraConfig = ''
                        #     proxy_set_header Host localhost";
                        # '';
                    };
                    # "= /.client/manifest.json" = {
                    #     proxyPass = "https://localhost:3000/";
                    # };
                    # "~ /.client/a-zA-Z0-9_]+\\.png$" = ;
                    # "= /serviceworker.js" = ;
                };
            };
        };
    };
}
