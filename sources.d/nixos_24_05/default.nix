# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.05/nixos-24.05.1695.dd457de7e08c/nixexprs.tar.xz";
        sha256 = "15gnmmcyrb5fyhs8f0q5b7abxjla9i54zy7l6b3pvdazh9k3qf2y";
    };
    value = import nix_path;
}
