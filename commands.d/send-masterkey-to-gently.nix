{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    sshTarget = "root@${machine.targetHost}";
    escapeShellArg = nixpkgsCurrent.lib.strings.escapeShellArg;
    esc = escapeShellArg;
    escapeShellArgs = nixpkgsCurrent.lib.strings.escapeShellArgs;
    secrets_to_upload = attrValues (mapAttrs (name: value: { inherit name; } // value) machine.secrets);
    scriptToReceiveMasterKey = komputiloj.lib.writeCommand {
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
in komputiloj.lib.writeCommand {
    name = "send-keys-to-gently";
    runtimeInputs = [
        nixpkgsCurrent.packages.nix
        nixpkgsCurrent.packages.openssh
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
