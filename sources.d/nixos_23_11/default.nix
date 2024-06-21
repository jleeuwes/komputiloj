# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7665.03d771e513ce/nixexprs.tar.xz";
        sha256 = "0i021s8ab5616zq6as2d2wfvy7g5fmll4azyxwh46iglcbvln1df";
    };
    value = import nix_path;
}
