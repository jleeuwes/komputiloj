with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.05/nixos-22.05.4321.f10cdcf31dd/nixexprs.tar.xz";
        sha256 = "1kzj1si4nxd4xix6i87lf4hc6hgj6982zac7hbnawnvx6r1iqm3c";
    };
    value = import nix_path;
}
