capsules@{ boltons, nixos, command-platform, komputiloj, ... }:
with boltons;
let
    escapeShellArgs = nixos.lib.strings.escapeShellArgs;
    commandsList = attrNames komputiloj.all.commands;
in command-platform.local.packageBuilders.writeCommand {
    name = "help";
    text = ''
        echo "Available commands:"
        echo
        printf -- 'komputiloj %s\n' ${escapeShellArgs commandsList}
    '';
}
