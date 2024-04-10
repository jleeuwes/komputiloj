{ boltons, nixpkgsCurrent, komputiloj, all, ... }:
with boltons;
{
    commands.activate = import ./activation.nix {
        inherit boltons nixpkgsCurrent komputiloj all;
    };
}
