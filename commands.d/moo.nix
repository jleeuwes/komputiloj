{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
komputiloj.lib.writeCommand {
    name = "moo";
    runtimeInputs = [ nixpkgsCurrent.packages.cowsay ];
    text = ''
        cowsay 'MOO!'
    '';
}
