with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-23.05pre429057.42337aad353/nixexprs.tar.xz";
        sha256 = "0v9yr8yl0kl3g4h24jizakd36wqn7fh6izxfycyrbv0plzgdg7a7";
    };
    value = import nix_path;
}
