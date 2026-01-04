{ boltons, command-platform, nixos, ... }:
with boltons;
command-platform.local.packageBuilders.writeCommand {
    name = "moo";
    runtimeInputs = [ nixos.local.legacyPackages.cowsay ];
    text = ''
        if [[ $# -eq 0 ]]; then
            cowsay 'MOO!'
        else
            cowsay -e oO -- "$@"
        fi
    '';
}
