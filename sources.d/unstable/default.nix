with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-23.05pre452326.fab09085df1/nixexprs.tar.xz";
        sha256 = "1fq6qh6fyd6xndk84nvvbx02lgz321cgm58qwc74pv6wbrvpid03";
    };
    value = import nix_path;
}
