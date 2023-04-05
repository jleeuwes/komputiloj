# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.11/nixos-22.11.3539.3e01f83884b/nixexprs.tar.xz";
        sha256 = "1dpaxwpc5dnhp3ry8n6lvrx1wkjjilaabgqs698lhfw4vpbasi34";
    };
    value = import nix_path;
}
