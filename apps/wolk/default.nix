{ boltons, nextcloud, ... }:
{
    modules = {
        all_in_one = (import ./modules/nextcloud.nix) { inherit boltons nextcloud; };
    };
    
    nixopsKeys = {
        "nextcloud-admin" = {
            keyCommand = [ "wachtwoord" "cat" "-n" "secrets/admin@wolk.radstand.nl" ];
            user = "nextcloud";
            group = "nextcloud";
            permissions = "ug=r,o=";
        };
    };
}
