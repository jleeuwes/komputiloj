{ boltons, nixos_25_05, komputiloj-definitions, ... }:
with boltons.lib;
let
    inherit (nixos_25_05.lib.strings) escapeShellArg escapeShellArgs;
in rec {
    nixosModules = rec {
        JumpHost = {
            imports = [
                WarpUserOnHost
            ];
        };
        Ferrix = {
            imports = [
                WarpGateServiceOnFerrix
                SshClientConfig
            ];
        };
        WarpUserOnHost = { pkgs, ... }: {
            users.users.warp = {
                uid = 70008;
                group = "warp";
                isSystemUser = true;
                openssh.authorizedKeys.keys = let
                host_options = port: concatStringsSep "," [
                    # command executed if the user were to try a normal ssh login
                    # (won't actually do anything if the user has no shell configured)
                    "command=\"echo WELCOME TO WARP ZONE\""
                    # start by restricting everything
                    "restrict"
                    # then allow port-forwarding (only permitlisten does not work)
                    "port-forwarding"
                    # but disallow forward port-forwarding by only allowing an empty hostname
                    "permitopen=\":1234\""
                    # finally, allow reverse port-forwarding
                    "permitlisten=\"localhost:${toString port}\""
                ];
                user_options = concatStringsSep "," [
                    # command executed if the user were to try a normal ssh login
                    # (won't actually do anything if the user has no shell configured)
                    "command=\"echo WELCOME TO WARP ZONE\""
                    # start by restricting everything
                    "restrict"
                    # then allow port-forwarding
                    "port-forwarding"
                    # but disallow reverse port-forwarding by only allowing an empty hostname
                    "permitlisten=\":1234\""
                ];
                in [
                    "${host_options 42001} ${komputiloj-definitions.machines.ferrix.ssh.publicKey}"
                    # "${make_options 42002} ${komputiloj-definitions.machines.scarif.ssh.publicKey}"

                    "${user_options} ${komputiloj-definitions.users.jeroen.ssh.publicKeys.ferrix}"
                ];
            };
            users.groups.warp.gid = 70008;
        };
        # WarpUserOnClient = { pkgs, ... }: {
        #     users.users.warp = {
        #         uid = 70008;
        #         group = "warp";
        #         isSystemUser = true;
        #     };
        #     users.groups.warp.gid = 70008;
        # };
        WarpGateServiceOnFerrix = { pkgs, ... }: {
            systemd.services.warpgate = {
                serviceConfig = {
                    Type = "simple";
                    # User = "warp";
                    Restart = "on-failure";
                };
                wantedBy = [ "multi-user.target" ];
                path = [ pkgs.openssh ];
                script = ''
                    ssh -i /etc/ssh/ssh_host_ed25519_key -R 42001:localhost:22 warp@gently.radstand.nl -N
                '';
            };
        };
        SshClientConfig = {
            programs.ssh ={
                knownHosts = {
                    ferrix.publicKey = komputiloj-definitions.machines.ferrix.ssh.publicKey;
                };
                extraConfig = ''
                    Host ferrix
                        ProxyJump warp@gently.radstand.nl
                        Hostname localhost
                        Port 42001
                        HostKeyAlias ferrix
                '';
            };
        };
    };
}
