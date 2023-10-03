capsules@{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.scarif;
    esc = nixpkgsCurrent.lib.strings.escapeShellArg;
    escapeShellArgs = nixpkgsCurrent.lib.strings.escapeShellArgs;
    prefix = cname: name: if cname == "komputiloj" then name else "${cname}.${name}";
    listCapsule = cname: cvalue: attrValues (mapAttrs (name: value: prefix cname name) (cvalue.commands or {}));
    commandsList = concatLists (attrValues (mapAttrs listCapsule capsules));
in komputiloj.lib.writeCommand {
    name = "help";
    runtimeInputs = [ nixpkgsCurrent.packages.nix ];
    text = ''
        echo "Available commands:"
        echo
        printf -- 'komputiloj %s\n' ${escapeShellArgs commandsList}
    '';
}
