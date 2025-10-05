capsules@{ boltons, nixos_25_05, command-platform, komputiloj, ... }:
with boltons;
let
    escapeShellArgs = nixos_25_05.lib.strings.escapeShellArgs;
    commandsList = attrNames komputiloj.all.commands;
in command-platform.local.packageBuilders.writeCommand {
    name = "help";
    text = ''
        echo "Available commands:"
        echo
        printf -- 'komputiloj %s\n' ${escapeShellArgs commandsList}
    '';
}
