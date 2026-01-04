{ boltons, nixos, komputiloj-definitions }:
with boltons.lib;
let
    inherit (nixos.lib.strings) escapeShellArg escapeShellArgs;
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
                    # forward forwards:
                    "permitopen=\"localhost:42001\""
                    # reverse forwards:
                    "permitlisten=\"localhost:${toString port}\""
                ];
                in [
                    "${host_options 42001} ${komputiloj-definitions.machines.ferrix.ssh.publicKey}"
                    # "${make_options 42002} ${komputiloj-definitions.machines.scarif.ssh.publicKey}"
                ];
            };
            users.groups.warp.gid = 70008;
        };
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
                    ssh -i /etc/ssh/ssh_host_ed25519_key -R 42001:localhost:22 -L 42001:localhost:42001 warp@gently.radstand.nl -N
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
                        Hostname localhost
                        Port 42001
                        HostKeyAlias ferrix
                '';
            };
        };
    };
}
