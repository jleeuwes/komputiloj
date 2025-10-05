# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/nix-community/raspberry-pi-nix/archive/3e8100d5e976a6a2be363015cb33463af9ef441a.tar.gz";
        sha256 = "0hcv8z6kjs47sy7lhpwmjih6f2k2jkkfg7wsprirv533pvrg7m34";
    };
    value = import nix_path;
}
