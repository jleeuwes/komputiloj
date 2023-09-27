{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    esc = nixpkgsCurrent.lib.strings.escapeShellArg;
    sshTarget = "root@${machine.hostName}";
    sshCmd = "ssh ${esc sshTarget}";
in komputiloj.lib.writeCommand {
    name = "deploy-host-gently";
    text = ''
        new_toplevel=${machine.nixosSystem.config.system.build.toplevel}

        # TODO: nixops copies some (most?) things from https://cache.nixos.org
        # instead of through ssh
        nix-copy-closure --to ${esc sshTarget} "$new_toplevel"
        ${komputiloj.commands.send-keys-to-gently}
        ${sshCmd} "$new_toplevel"/bin/switch-to-configuration switch
    '';
}
