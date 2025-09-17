{ komputiloj-privata, hello-infra, ... }:
# proper module starts here
{ pkgs, config, lib, ... }:
{

    _file = __curPos.file;

    imports = [
        komputiloj-privata.modules.ssh-client-config
        hello-infra.modules.ssh-client-config
    ];
    
    programs.ssh = {
        extraConfig = ''
            Host gently
                Hostname gently.radstand.nl
            Host scarif
                Hostname scarif.radstand.nl
        '';
        knownHosts = {
            "thee.radstand.nl" = {
                # Needed for gorinchemindialoog autocommit.
                # I guess this key is regenerated on gitea install, so we'll have to update this if rebuilding.
                publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHmMPh91t1reE1ddLcFYyddQs0hx4v41KcaNBS2UVnEA";
            };
        };
    };
}
