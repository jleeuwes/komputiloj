# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/63209b1def2c9fc891ad271f474a3464a5833294/nixos-mailserver-63209b1def2c9fc891ad271f474a3464a5833294.tar.gz";
        sha256 = "05k4nj2cqz1c5zgqa0c6b8sp3807ps385qca74fgs6cdc415y3qw";
    };
    value = import nix_path;
}
