{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
komputiloj.lib.writeCommand {
    name = "moo";
    runtimeInputs = [ nixpkgsCurrent.packages.cowsay ];
    text = ''
        if [[ $# -eq 0 ]]; then
            cowsay -f llama "Oké, maar als er iets is waar je over wil praten, dan ben ik er voor je!"
        else
            cowsay -f llama -- "$@"
        fi
    '';
}
