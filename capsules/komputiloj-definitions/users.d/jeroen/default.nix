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
    apps.notie = {
        spaces = [ "hello" ];
    };
    apps.knol = { # will replace radicale.users
        enable = true;
    };
    apps.sleutel = {
        enable = true;
    };
    linux.uid = 1000;
    email = "jeroen@lwstn.eu";
    isHuman = true;
    
    # TODO replace usages of this:
    sshKeys = {
        ferrix = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDWHWQVPZcUu5B1CAaobojr8VHa3/MZQhxRHrPWQIZrq
        '';
    };

    # with this (used by thee):
    ssh.publicKeys = sshKeys;
}
