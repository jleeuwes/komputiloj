{ boltons, command-platform, nixos, komputiloj, ... }:
with boltons;
let
    machine = komputiloj.machines.ferrix;
    esc = nixos.lib.strings.escapeShellArg;
in command-platform.local.packageBuilders.writeCommand {
    name = "deploy-on-ferrix";
    runtimeInputs = [ nixos.local.legacyPackages.nix ];
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
