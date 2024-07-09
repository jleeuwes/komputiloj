{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    sshTarget = "root@${machine.targetHost}";
    escapeShellArg = nixpkgsCurrent.lib.strings.escapeShellArg;
    esc = escapeShellArg;
    escapeShellArgs = nixpkgsCurrent.lib.strings.escapeShellArgs;
    keys_to_upload = attrValues (mapAttrs (name: value: { inherit name; } // value) machine.nixopsKeys);
    secrets_to_upload = attrValues (mapAttrs (name: value: { inherit name; } // value) machine.secrets);
    scriptToPrepareKeyUpload = komputiloj.lib.writeCommand {
        name = "pre-receive-keys";
        # The upload dir acts as:
        # 1. A temporary place only root can read while we fiddle with the
        #    secret files.
        # 2. A lock to prevent concurrent key uploading.
        text = ''
            mkdir /run/keys/upload # this dir also acts as a lock
            chmod go= /run/keys/upload
        '';
    };
    scriptToReceiveKey = key: let
        destDir = key.destDir or "/run/keys";
        user = key.user or "root"; # NOTE: we bypass the nixops module system here
        group = key.group or "root"; # NOTE: we bypass the nixops module system here
        permissions = key.permissions or "0600"; # NOTE: we bypass the nixops module system here
    in komputiloj.lib.writeCommand {
        name = "receive-key-${key.name}";
        text = ''
            cat > /run/keys/upload/${esc key.name}
            chown -- ${esc user}:${esc group} /run/keys/upload/${esc key.name}
            chmod -- ${esc permissions} /run/keys/upload/${esc key.name}
            mkdir -p -- ${esc destDir}
            mv -- /run/keys/upload/${esc key.name} ${esc destDir}
        '';
    };
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
    scriptToDecryptSecret = secret: let
        # TODO put this in the activation script instead
        destDir = secret.destDir or "/run/keys";
        user = secret.user or "root"; # NOTE: we bypass the nixops module system here
        group = secret.group or "root"; # NOTE: we bypass the nixops module system here
        permissions = secret.permissions or "0600"; # NOTE: we bypass the nixops module system here
    in komputiloj.lib.writeCommand {
        name = "decrypt-secret-${secret.name}";
        text = ''
            mkdir -p -- ${esc destDir}
            keyfile=${esc destDir}/${esc secret.name}
            rm -f -- "$keyfile".new
            set -o noclobber
            umask u=rw,go=
            ${nixpkgsCurrent.packages.age}/bin/age --decrypt \
                -i /run/keys/masterkey \
                ${nixpkgsCurrent.lib.writeText secret.name secret.encryptedContent} \
                > "$keyfile".new
            chown -- ${esc user}:${esc group} "$keyfile".new
            chmod -- ${esc permissions} "$keyfile".new
            mv -T -- "$keyfile".new "$keyfile"
        '';
    };
    scriptToFinishKeyUpload = komputiloj.lib.writeCommand {
        name = "post-receive-keys";
        text = ''
            rmdir /run/keys/upload
            # touch /run/keys/done # TODO
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

        nix-copy-closure --to ${esc sshTarget} \
            ${scriptToPrepareKeyUpload} \
            ${scriptToReceiveMasterKey} \
            ${scriptToFinishKeyUpload} \
            ${unwords (map (key: "${scriptToReceiveKey key}") keys_to_upload)} \
            ${unwords (map (key: "${scriptToDecryptSecret key}") secrets_to_upload)}
        
        ${sshCmd} ${scriptToPrepareKeyUpload}
        
        (
        # This part needs a specific working dir for running the keyCommands
        cd -- "$KOMPUTILOJ_PATH"
        ${unlines (map
            (key: "${escapeShellArgs key.keyCommand}"
                + " | "
                + "${sshCmd} ${scriptToReceiveKey key}")
            keys_to_upload)}
        )
        
        # WARNING: don't use masterAgeKey in string interpolation,
        # because it's copied to the nix store if you do
        ${komputiloj.packages.wachtwoord}/bin/wachtwoord cat \
            ${toString machine.masterAgeKey.secretKeyFile} \
            | ${sshCmd} ${scriptToReceiveMasterKey}
        ${unlines (map
            (key: "${sshCmd} ${scriptToDecryptSecret key}")
            secrets_to_upload)}

        ${sshCmd} ${scriptToFinishKeyUpload}
    '';
}
