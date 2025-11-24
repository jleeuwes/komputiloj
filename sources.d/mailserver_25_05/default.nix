# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/5b38fb599f50e9d78325d1d2706e36303c166047/nixos-mailserver-5b38fb599f50e9d78325d1d2706e36303c166047.tar.gz";
        sha256 = "0la8v8d9vzhwrnxmmyz3xnb6vm76kihccjyidhfg6qfi3143fiwq";
    };
    value = import nix_path;
}
