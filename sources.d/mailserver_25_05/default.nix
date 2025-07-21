# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/53007af63fade28853408370c4c600a63dd97f41/nixos-mailserver-53007af63fade28853408370c4c600a63dd97f41.tar.gz";
        sha256 = "0jpp086m839dz6xh6kw5r8iq0cm4nd691zixzy6z11c4z2vf8v85";
    };
    value = import nix_path;
}
