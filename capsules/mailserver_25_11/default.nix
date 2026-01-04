{ boltons, platform, nixos_25_11 }:
with boltons.lib;
let
pins = importDirAndApply ./pins.d {
    lib = nixos_25_11.lib;
    legacyPackages = nixos_25_11.native.${platform.localSystem}.legacyPackages;
};
self = {
    all = platform.lib.makeAll self;

    inherit pins;
    
    nixosModules = {
        mailserver = import pins.nixos-mailserver.lock.nix_path;
    };
    
}; in self
