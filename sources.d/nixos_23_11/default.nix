# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.4030.9f2ee8c91ac4/nixexprs.tar.xz";
        sha256 = "1ngsfxmhvm6zp1wzjl1z69q9hv4l2kbm99dbhgfjr4py9wpiwwsk";
    };
    value = import nix_path;
}
