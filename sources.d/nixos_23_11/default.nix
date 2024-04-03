# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.5742.219951b495fc/nixexprs.tar.xz";
        sha256 = "0710zalhzcfw725lj8rryx2vhj1mjcxllvxiz53pdqq4l6rw63vn";
    };
    value = import nix_path;
}
