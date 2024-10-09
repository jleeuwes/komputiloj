{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.ferrix;
    esc = nixpkgsCurrent.lib.strings.escapeShellArg;
in komputiloj.lib.writeCommand {
    name = "deploy-on-ferrix";
    runtimeInputs = [ nixpkgsCurrent.packages.nix ];
    text = ''
        if [ "$HOSTNAME" != ferrix ]; then
            echo "This config is supposed to be deployed on ferrix, not $HOSTNAME" >&2
            exit 1
        fi
        # TODO maybe we should use https://nixos.wiki/wiki/Nixos-rebuild
        # instead of doing the steps ourselves? Not sure.
        new_toplevel=${machine.nixosSystem.config.system.build.toplevel}
        sudo nix-env -p /nix/var/nix/profiles/system --set "$new_toplevel"
        sudo "$new_toplevel"/bin/switch-to-configuration switch
    '';
}
