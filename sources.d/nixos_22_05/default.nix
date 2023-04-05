# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.05/nixos-22.05.4690.08741686397/nixexprs.tar.xz";
        sha256 = "09zikzglazai016hyjhby8dc0s4bld6s7yzb2x62a0kakbsp9414";
    };
    value = import nix_path;
}
