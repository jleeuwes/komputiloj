# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.4112.5a237aecb572/nixexprs.tar.xz";
        sha256 = "1jy1dbh46mkyv2aaqchnp366c3ssifym52clnhafs0647lskyj2s";
    };
    value = import nix_path; # import (nix_path + "/pkgs/top-level");
}
