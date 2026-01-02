{ boltons, command-platform, nixos_25_05, ... }:
with boltons;
command-platform.local.packageBuilders.writeCommand {
    name = "moo";
    runtimeInputs = [ nixos_25_05.local.legacyPackages.cowsay ];
    text = ''
        if [[ $# -eq 0 ]]; then
            cowsay -f llama "Oké, maar als er iets is waar je over wil praten, dan ben ik er voor je!"
        else
            cowsay -f llama -- "$@"
        fi
    '';
}
