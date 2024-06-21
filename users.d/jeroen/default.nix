with builtins;
{
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
    sshKeys = {
        # TODO place the key besides this nix file? But then where do we put the private key?
        scarif = readFile ../../machines.d/scarif/home/jeroen/.ssh/id_rsa.pub;
    };
}
