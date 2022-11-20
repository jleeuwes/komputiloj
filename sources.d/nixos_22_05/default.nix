with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.05/nixos-22.05.3935.b3a8f7ed267/nixexprs.tar.xz";
        sha256 = "0hkhj1sas3dzymx2jsm0dz0zh1lq0snqwi1khpjh2q4asicc4xv7";
    };
    value = import nix_path;
}
