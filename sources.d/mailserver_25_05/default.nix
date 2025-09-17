# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/f5936247dbdb8501221978562ab0b302dd75456c/nixos-mailserver-f5936247dbdb8501221978562ab0b302dd75456c.tar.gz";
        sha256 = "1qn5fg0h62r82q7xw54ib9wcpflakix2db2mahbicx540562la1y";
    };
    value = import nix_path;
}
