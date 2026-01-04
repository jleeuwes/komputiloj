{ nixos_future }: {
    modules = rec {
        all_in_one = {
            imports = [
                _silverbullet
                _users
                _secrets
                _nginx
            ];
        };
        _silverbullet = {config, pkgs, ...}: {
            services.silverbullet = {
                enable = true; # DO NOT ENABLE WITHOUT SETTING A BASICAUTH PASSWORD BELOW
                package = nixos_future.native.${pkgs.system}.packages.silverbullet;
                # currently only one space is supported
                spaceDir = "/mnt/storage/live/silverbullet/spaces/hello";
                envFile = "/run/silverbullet.hello.env";
                listenPort = 3010;
            };
            systemd.services.silverbullet = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                requires = [ "silverbullet-env.service" ];
                after = [ "silverbullet-env.service" ];
            };
        };
        _users = {
            users.users.silverbullet.uid = 70006;
            users.groups.silverbullet.gid = 70006;
        };
        _secrets = { pkgs, ... }: {
            # secrets = {
            #     silverbullet-token = {
            #         encryptedContent = komputiloj-privata.secrets.silverbullet-token.encryptedContent;
            #         user = "silverbullet";
            #         group = "silverbullet";
            #         permissions = "u=r,go=";
            #     };
            # };
            systemd.services.silverbullet-token = {
                serviceConfig = {
                    Type = "oneshot";
                    RemainAfterExit = true;
                };
                path = [ pkgs.pwgen ];
                script = ''
                    if [[ ! -e /run/silverbullet.token ]]; then
                        umask u=rw,go=
                        pwgen -s 50 > /run/silverbullet.token
                        chown silverbullet:silverbullet /run/silverbullet.token
                        chmod ug=r,o= /run/silverbullet.token
                    fi
                '';
            };
            systemd.services.silverbullet-env = {
                serviceConfig = {
                    Type = "oneshot";
                    RemainAfterExit = true;
                };
                requires = [ "silverbullet-token.service" ];
                after = [ "silverbullet-token.service" ];
                script = ''
                    SB_AUTH_TOKEN=$(cat /run/silverbullet.token)
                    umask u=rw,go=
                    cat > /run/silverbullet.hello.env <<EOF
                    SB_KV_DB=/mnt/storage/live/silverbullet/spaces/hello.sqlite
                    # SB_USER needs to be set for SB_AUTH_TOKEN to have effect!
                    SB_USER=user
                    SB_AUTH_TOKEN=$(printf '%q' "$SB_AUTH_TOKEN")
                    EOF
                    chown silverbullet:silverbullet /run/silverbullet.hello.env
                    chmod ug=r,o= /run/silverbullet.hello.env
                '';
            };
            systemd.services.silverbullet-nginxvars = {
                serviceConfig = {
                    Type = "oneshot";
                    RemainAfterExit = true;
                };
                requires = [ "silverbullet-token.service" ];
                after = [ "silverbullet-token.service" ];
                script = ''
                    SB_AUTH_TOKEN=$(cat /run/silverbullet.token)
                    umask u=rw,go=
                    cat > /run/silverbullet.nginxvars <<EOF
                    set \$silverbullet_token "$SB_AUTH_TOKEN";
                    EOF
                    chown nginx:nginx /run/silverbullet.nginxvars
                    chmod ug=r,o= /run/silverbullet.nginxvars
                '';
            };
        };
        _nginx = {
            users.users.nginx = {
                extraGroups = [ "silverbullet" ];
            };
            systemd.services.nginx = {
                requires = [ "silverbullet-nginxvars.service" ];
                after = [ "silverbullet-nginxvars.service" ];
            };
            services.nginx.virtualHosts."notie.radstand.nl" = {
                forceSSL = true;
                enableACME = true;
                # NOTE: nginx does not depend on mounted storage; what does this do if the basicAuthFile is not available?
                basicAuthFile = "/mnt/storage/live/sleutel/rootdir/apps/notie/hello/auth";
                locations = {
                    "/" = {
                        proxyPass = "http://localhost:3010/";
                        extraConfig = ''
                            include /run/silverbullet.nginxvars;
                            proxy_set_header Authorization "Bearer $silverbullet_token";
                        '';
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
