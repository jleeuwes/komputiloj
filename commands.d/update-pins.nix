{ boltons, command-platform, nixos_25_05, all, ... }:
with boltons.lib;
let
    machine = komputiloj.machines.gently;
    esc = nixos_25_05.lib.strings.escapeShellArg;
    sshTarget = "root@${machine.targetHost}";
    sshCmd = "ssh ${esc sshTarget}";
in command-platform.local.packageBuilders.writeCommand {
    name = "update-pins";
    # TODO: Make separate scripts and call those.
    #       Currently we concatenate the updateScripts together
    #       which obviously does not isolate capsules enough from each other
    #       and which we can only get away with because we trust our capsules.
    text = ''
        ${unlines (attrValues (mapAttrs
            (qualified_name: pin:
                if pin ? updateScript
                then
                    "printf 'Updating pin %s\\n' ${esc qualified_name} >&2"
                    + "\n" +
                    "${pin.updateScript}"
                    + "\n"
                else
                    "printf 'Skipping pin without updateScript: %s\\n' ${esc qualified_name} >&2"
                    + "\n"
            )
            all.pins))}
    '';
}
