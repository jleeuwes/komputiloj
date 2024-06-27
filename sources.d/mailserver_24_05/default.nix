# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/29916981e7b3b5782dc5085ad18490113f8ff63b/nixos-mailserver-29916981e7b3b5782dc5085ad18490113f8ff63b.tar.gz";
        sha256 = "0clvw4622mqzk1aqw1qn6shl9pai097q62mq1ibzscnjayhp278b";
    };
    value = import nix_path;
}
