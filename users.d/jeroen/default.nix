with builtins;
rec {
    name = "jeroen";
    fullName = "Jeroen Leeuwestein";
    apps.thee = {
        enable = true;
        visibility = "public";
    };
    apps.wolk = {
        bigstorage = true;
    };
    linux.uid = 1000;
    email = "jeroen@lwstn.eu";
    isHuman = true;
    
    # TODO replace usages of this:
    sshKeys = {
        # TODO place the key besides this nix file? But then where do we put the private key?
        scarif = readFile ../../machines.d/scarif/home/jeroen/.ssh/id_rsa.pub;
        ferrix = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDWHWQVPZcUu5B1CAaobojr8VHa3/MZQhxRHrPWQIZrq
        '';
    };

    # with this (used by thee):
    ssh.publicKeys = sshKeys;
}
