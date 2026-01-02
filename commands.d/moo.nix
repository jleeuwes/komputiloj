{ boltons, command-platform, nixos_25_05, ... }:
with boltons;
command-platform.local.packageBuilders.writeCommand {
    name = "moo";
    runtimeInputs = [ nixos_25_05.local.legacyPackages.cowsay ];
    text = ''
        if [[ $# -eq 0 ]]; then
            cowsay 'MOO!'
        else
            cowsay -e oO -- "$@"
        fi
    '';
}
