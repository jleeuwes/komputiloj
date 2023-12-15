# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.5184.946310306972/nixexprs.tar.xz";
        sha256 = "18wlrq100n210pv0b15c1xv68mxc85finn07ff61fliy14arfrqh";
    };
    value = import nix_path;
}
