# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.2171.0b3d61817311/nixexprs.tar.xz";
        sha256 = "0wnxyfjbb8wbf9kw3z8ajxjwk0myani615272s9vyppz8vd6g4qv";
    };
    value = import nix_path;
}
