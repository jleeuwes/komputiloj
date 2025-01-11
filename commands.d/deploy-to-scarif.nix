{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.scarif;
    esc = nixpkgsCurrent.lib.strings.escapeShellArg;
    sshTarget = "root@${machine.targetHost}";
    sshCmd = "ssh ${esc sshTarget}";
in komputiloj.lib.writeCommand {
    name = "deploy-to-scarif";
    runtimeInputs = [ nixpkgsCurrent.packages.openssh ];
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
