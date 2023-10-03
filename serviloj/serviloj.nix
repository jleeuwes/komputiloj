# nixops compatibility layer
with builtins;
let
    topLevel = import ../.;
    machine = topLevel.capsules.komputiloj.machines.gently;
in {
    network.description = "Our humble all-encompassing serviloj deployment";
    network.nixpkgs = topLevel.capsules.nixpkgsCurrent.packages;
    gently2 = { config, lib, pkgs, ...}:
        (machine.mainModule {
            inherit config lib pkgs;
        }) // {
            deployment.targetHost = machine.targetHost;
            deployment.provisionSSHKey = false;
            # deployment.hasFastConnection = true; # helps to deploy when DNS is borked on the server
            deployment.keys = machine.nixopsKeys;
        };
}

