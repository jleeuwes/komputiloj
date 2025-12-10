{ boltons, nixos_25_05, command-platform, komputiloj, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    sshTarget = "root@${machine.targetHost}";
    targetSystem = "x86_64-linux";
    escapeShellArg = nixos_25_05.lib.strings.escapeShellArg;
    esc = escapeShellArg;
    escapeShellArgs = nixos_25_05.lib.strings.escapeShellArgs;
    scriptToReceiveMasterKey = command-platform.native.${targetSystem}.packageBuilders.writeCommand {
        name = "receive-masterkey";
        text = ''
            rm -f /run/keys/masterkey.new
            set -o noclobber # makes sure the cat below actually creates the
                             # file, to prevent writing to any existing file
                             # (created concurrently somehow) with wrong
                             # permissions set
            umask u=rw,go=   # makes sure the file gets the correct permissions
                             # right away
            cat > /run/keys/masterkey.new
            test -s /run/keys/masterkey.new # if something fails on the client
                             # side, such as a decryption error,
                             # the cat does not fail but leaves an empty file;
                             # so we check for that
            mv /run/keys/masterkey{.new,}
        '';
    };
    sshCmd = "ssh ${esc sshTarget}";
in command-platform.local.packageBuilders.writeCommand {
    name = "send-keys-to-gently";
    runtimeInputs = [
        nixos_25_05.local.legacyPackages.nix
        nixos_25_05.local.legacyPackages.openssh
        komputiloj.packages.wachtwoord
    ];
    text = ''
        [[ -n "$KOMPUTILOJ_PATH" ]]

        nix-copy-closure --to ${esc sshTarget} ${scriptToReceiveMasterKey}
        
        # WARNING: don't use masterAgeKey in string interpolation,
        # because it's copied to the nix store if you do
        ${komputiloj.packages.wachtwoord}/bin/wachtwoord cat \
            ${toString machine.masterAgeKey.secretKeyFile} \
            | ${sshCmd} ${scriptToReceiveMasterKey}
    '';
}
