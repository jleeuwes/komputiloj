# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.3037.f155f0cf4ea4/nixexprs.tar.xz";
        sha256 = "11p13m3zfzpbcha6l82iibvddxbcmhnf1jljl9jmrlspiicvikc5";
    };
    value = import nix_path;
}
