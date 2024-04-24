# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.6478.bc194f70731c/nixexprs.tar.xz";
        sha256 = "0gdp6a2jbv7lfxzwlmw4ck2g6bdgsj8hib8gq53wrjam4q547z4s";
    };
    value = import nix_path;
}
