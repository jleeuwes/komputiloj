# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.2084.08700de174b/nixexprs.tar.xz";
        sha256 = "13gfbqlwpbr6k8hlpdlhji4nhgbgr5czrdv65al7zrdllpfvcmdl";
    };
    value = import nix_path;
}
