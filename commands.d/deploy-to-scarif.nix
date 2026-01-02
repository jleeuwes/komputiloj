{ boltons, komputiloj, command-platform, nixos_25_05, ... }:
with boltons;
let
    machine = komputiloj.machines.scarif;
    esc = nixos_25_05.lib.strings.escapeShellArg;
    sshTarget = "root@${machine.targetHost}";
    sshCmd = "ssh ${esc sshTarget}";
in command-platform.local.packageBuilders.writeCommand {
    name = "deploy-to-scarif";
    runtimeInputs = [ nixos_25_05.local.legacyPackages.openssh ];
    text = ''
        new_toplevel=${machine.nixosSystem.config.system.build.toplevel}

        nix-copy-closure \
            --to ${esc sshTarget} \
            --gzip --use-substitutes \
            "$new_toplevel"
        # We assume nix-env is present on the remote machine.
        ${sshCmd} nix-env -p /nix/var/nix/profiles/system --set "$new_toplevel"
        ${sshCmd} "$new_toplevel"/bin/switch-to-configuration switch
    '';
}
