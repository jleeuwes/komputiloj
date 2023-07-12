# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.11/nixos-22.11.4773.ea4c80b39be/nixexprs.tar.xz";
        sha256 = "03y1dg26wzy5bv4ix7pd33jbfyl6ncjl7s9cyf83clfvj9g7p7gk";
    };
    value = import nix_path;
}
