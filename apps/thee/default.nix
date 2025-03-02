{ boltons, nixpkgsCurrent, komputiloj, all, ... }:
with boltons;
let
    # NOTE: after changing the stateDir, regenerate gitea's authorized_keys file through the admin webinterface.
    stateDir = "/mnt/storage/live/gitea/rootdir";
in {
    commands.activate = import ./activation.nix {
        inherit boltons nixpkgsCurrent komputiloj all;
    };

    modules.all_in_one = {
        services.gitea.enable = false;
        services.forgejo = {
            enable = true;

            user = "gitea";
            group = "gitea";

            # NOTE: after changing the stateDir, regenerate gitea's authorized_keys file through the admin webinterface.
            stateDir = "${stateDir}";

            database = {
                type = "sqlite3";
                createDatabase = false;
                path = "${stateDir}/data/gitea.db";
            };

            # mailerPasswordFile = ...;
            settings = {
                server = {
                    ROOT_URL = "https://thee.radstand.nl/";
                    DOMAIN = "thee.radstand.nl";
                };
                mailer = {
                    ENABLED = true;
                    FROM = "thee@radstand.nl";
                    # https://docs.gitea.io/en-us/config-cheat-sheet/#mailer-mailer
                    SMTP_ADDR = "localhost";
                    SMTP_PORT = "25";
                    FORCE_TRUST_SERVER_CERT = true; # this is okay, as long as it's localhost
                    # https://github.com/NixOS/nixpkgs/issues/103446
                    # MAILER_TYPE = "sendmail"; # not sure which of...
                    # PROTOCOL = "sendmail";    # ...these two we need
                    # SENDMAIL_PATH = "${pkgs.system-sendmail}/bin/sendmail";
                };
                service = {
                    DISABLE_REGISTRATION = true;
                    ENABLE_NOTIFY_MAIL = true;
                };
                log = {
                    LEVEL = "Info";
                };
                session = {
                    COOKIE_SECURE = true;
                };
                "ssh.minimum_key_sizes" = {
                    # TODO remove when scarif is taken out of commission
                    RSA = 2048;
                };
                "cron.git_gc_repos" = {
                    ENABLED = true;
                    SCHEDULE = "@every 72h";
                    TIMEOUT = "15m";
                    NOTICE_ON_SUCCESS = true;
                };
                other = {
                    SHOW_FOOTER_VERSION = false;
                };
            };
        };
        systemd.services.forgejo = {
            needsStorageVolume = "requires";
        };
        systemd.services.forgejo-secrets = {
            needsStorageVolume = "requires";
        };
        users.users.gitea = {
            uid = 70001;
            group = "gitea";
            isSystemUser = true;
            # Taken from https://github.com/NixOS/nixpkgs/blob/nixos-23.11/nixos/modules/services/misc/forgejo.nix
            home = stateDir; # needed to make authorized_keys work
            useDefaultShell = true;
        };
        users.groups.gitea = {
            gid = 70001;
        };
        services.nginx = {
            virtualHosts = {
                "thee.radstand.nl" = {
                    forceSSL = true;
                    enableACME = true;
                    locations."/" = {
                        proxyPass = "http://localhost:3000/";
                    };
                };
            };
        };
    };

    packages.forgejo-cli = nixpkgsCurrent.packages.writeShellApplication {
        name = "thee-forgejo";
        runtimeInputs = [ nixpkgsCurrent.packages.forgejo ];
        text = ''
            if [[ $# -eq 0 ]]; then
                echo "gitea without arguments would run the web app." >&2
                echo "It's highly unlikely that you want to run the web app this way." >&2
                echo "Please give a command." >&2
                exit 1
            fi
            
            # not sure which of these gets picked up:
            export GITEA_WORK_DIR=${stateDir}
            export GITEA_CUSTOM=${stateDir}/custom
            export FORGEJO_WORK_DIR=${stateDir}
            export FORGEJO_CUSTOM=${stateDir}/custom
            
            sudo --preserve-env=GITEA_WORK_DIR,GITEA_CUSTOM,FORGEJO_WORK_DIR,FORGEJO_CUSTOM -u gitea gitea "$@"
        '';
    };
}
