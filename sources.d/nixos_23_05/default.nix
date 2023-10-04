# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.4065.3b79cc4bcd9c/nixexprs.tar.xz";
        sha256 = "1krxdffmcxk9myjn6zaqsks424bbq510ljyckslmf2dsc3nc47yd";
    };
    value = import nix_path;
}
