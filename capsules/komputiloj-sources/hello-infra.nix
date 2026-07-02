# attempt at packaging a nix expression from a private git repo,
# based on https://phip1611.de/blog/accessing-network-from-a-nix-derivation/
{ packageBuilders, ... }:
packageBuilders.mkDerivation {
    name = "hello-infra";
    src = builtins.fetchGit {
        url = "gitea@thee.radstand.nl:hello/infra.git";
        rev = "fec9955b19e44742d32a87a4233ed9797ce837c9";
        submodules = true;
        
        # ref prevents an SSH attempt somehow.
        # Without it, a connection is made to `url` even if this package is
        # already built and present in the nix store.
        ref = "main";
    };
    doCheck = false;
    dontFixup = true;
    buildPhase = ":";
    installPhase = ''
        mkdir $out
        cp -r . $out
    '';
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-pjhfUNSgL8IQxi0i0y92i0E+BXx5d4Aq8qEguM8G9YU=";
}
