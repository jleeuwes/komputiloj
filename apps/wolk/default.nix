{ boltons, nextcloud, nixpkgsCurrent, ... }:
{
    modules = {
        all_in_one = (import ./modules/nextcloud.nix) { inherit boltons nextcloud nixpkgsCurrent; };
    };
    
    nixopsKeys = {
        "nextcloud-admin" = {
            keyCommand = [ "wachtwoord" "cat" "-n" "secrets/admin@wolk.radstand.nl" ];
            user = "nextcloud";
            group = "nextcloud";
            permissions = "ug=r,o=";
        };
        "bigstorage1-wolk-webdav-pass" = {
            keyCommand = [ "wachtwoord" "cat" "-n" "secrets/bigstorage1-wolk@hetzner" ];
            user = "nextcloud";
            group = "nextcloud";
            permissions = "ug=r,o=";
        };
        "bigstorage1-wolk-crypt-pass" = {
            keyCommand = [ "wachtwoord" "cat" "-n" "secrets/bigstorage1-wolk-crypt-pass@hetzner" ];
            user = "nextcloud";
            group = "nextcloud";
            permissions = "ug=r,o=";
        };
        "bigstorage1-wolk-crypt-salt" = {
            keyCommand = [ "wachtwoord" "cat" "-n" "secrets/bigstorage1-wolk-crypt-salt@hetzner" ];
            user = "nextcloud";
            group = "nextcloud";
            permissions = "ug=r,o=";
        };
    };
}
