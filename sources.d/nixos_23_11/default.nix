# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.4801.b7ee09cf5614/nixexprs.tar.xz";
        sha256 = "0y855q2d8pwgd2vkpvsw7022ajkjki0yha75aifa3rqrhr8w2aij";
    };
    value = import nix_path;
}
