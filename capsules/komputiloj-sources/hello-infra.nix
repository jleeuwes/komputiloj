# attempt at packaging a nix expression from a private git repo,
# based on https://phip1611.de/blog/accessing-network-from-a-nix-derivation/
{ packageBuilders, ... }:
packageBuilders.mkDerivation {
    name = "hello-infra";
    src = builtins.fetchGit {
        url = "gitea@thee.radstand.nl:hello/infra.git";
        rev = "3f31bd0dcef8af8380e916b90e72973ecdaa2086";
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
    outputHash = "sha256-xjGbGzISklqp5dT+8aPWCGXI8E7NMG+nEIPlUjQsPUI=";
}
