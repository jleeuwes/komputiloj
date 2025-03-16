{ boltons, nextcloud, nixpkgsCurrent, privata, all, ... }:
{
    modules = rec {
        all_in_one = {
            imports = [ nextcloud_module secrets_module ];
        };
        nextcloud_module = (import ./modules/nextcloud.nix) {
            inherit boltons nextcloud nixpkgsCurrent;
            users = all.users;
        };
        secrets_module = {
            secrets = {
                nextcloud-admin-password = {
                    encryptedContent = privata.secrets.nextcloud-admin-password.encryptedContent;
                    user = "nextcloud";
                    group = "nextcloud";
                    permissions = "ug=r,o=";
                };
                bigstorage1-wolk-webdav-pass = {
                    encryptedContent = privata.secrets.bigstorage1-wolk-webdav-pass.encryptedContent;
                    user = "nextcloud";
                    group = "nextcloud";
                    permissions = "ug=r,o=";
                };
                bigstorage1-wolk-crypt-pass = {
                    encryptedContent = privata.secrets.bigstorage1-wolk-crypt-pass.encryptedContent;
                    user = "nextcloud";
                    group = "nextcloud";
                    permissions = "ug=r,o=";
                };
                bigstorage1-wolk-crypt-salt = {
                    encryptedContent = privata.secrets.bigstorage1-wolk-crypt-salt.encryptedContent;
                    user = "nextcloud";
                    group = "nextcloud";
                    permissions = "ug=r,o=";
                };
            };
        };
    };
    
}
