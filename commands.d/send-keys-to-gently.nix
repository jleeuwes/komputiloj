{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    sshTarget = "root@${machine.targetHost}";
    escapeShellArg = nixpkgsCurrent.lib.strings.escapeShellArg;
    esc = escapeShellArg;
    escapeShellArgs = nixpkgsCurrent.lib.strings.escapeShellArgs;
    keys_to_upload = attrValues (mapAttrs (name: value: { inherit name; } // value) machine.nixopsKeys);
    prepareKeyUploadDirScript = komputiloj.lib.writeCommand {
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
    receiveKeyScript = key: let
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
    finishKeyUploadScript = komputiloj.lib.writeCommand {
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
            ${prepareKeyUploadDirScript} \
            ${finishKeyUploadScript} \
            ${unwords (map (key: "${receiveKeyScript key}") keys_to_upload)}
        
        ${sshCmd} ${prepareKeyUploadDirScript}
        
        (
        cd -- "$KOMPUTILOJ_PATH"
        ${unlines (map
            (key: "${escapeShellArgs key.keyCommand}"
                + " | "
                + "${sshCmd} ${receiveKeyScript key}")
            keys_to_upload)}
        )

        ${sshCmd} ${finishKeyUploadScript}
    '';
}
