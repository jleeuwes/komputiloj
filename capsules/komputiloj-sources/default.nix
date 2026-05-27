{ nixos }:
{
    portable.packages = {
        hello-infra = import ./hello-infra.nix nixos.local;
    };
}
