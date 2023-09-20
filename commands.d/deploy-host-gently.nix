{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
let
    machine = komputiloj.machines.gently;
    escapeShellArg = nixpkgsCurrent.lib.strings.escapeShellArg;
in komputiloj.lib.writeCommand {
    name = "deploy-host-gently";
    text = ''
        new_toplevel=${machine.nixosSystem.config.system.build.toplevel}
        target=${escapeShellArg machine.hostName}

        nix-copy-closure --to root@"$target" "$new_toplevel"
        # TODO we need to add key upload before we can activate the configuration
        # ssh root@"$target" "$new_toplevel"/bin/switch-to-configuration switch

        echo "Stopping now. I'm not implemented yet."
    '';
    # runtimeInputs = [
    #     nixpkgsCurrent.packages.openssh
    #     komputiloj.packages.wachtwoord
    # ];
    # text = ''
    #     [[ -n "$KOMPUTILOJ_PATH" ]]
    #     set -u

    #     upload_key() {
    #         local local_name="$1"
    #         local remote_file="$2"
    #         local remote_command
    #         remote_command=$(printf 'cat > %q' "$remote_file")
    #         # TODO move secrets out of serviloj
    #         # shellcheck disable=SC2029
    #         wachtwoord cat -n \
    #             "$KOMPUTILOJ_PATH"/serviloj/secrets/"$local_name" | \
    #             ssh root@${machine.hostName} "$remote_command"
    #     }

    #     key_upload_dir=/run/keys/upload."$RANDOM"
    #     # shellcheck disable=SC2087
    #     ssh root@${machine.hostName} bash - <<-EOF
    #         mkdir $key_upload_dir
    #         chmod go= $key_upload_dir
    #     EOF

    #     upload_key luks-storage@hetzner "$key_upload_dir"/luks-storage

    #     # shellcheck disable=SC2087
    #     ssh root@${machine.hostName} bash - <<-EOF
    #         mv $key_upload_dir/* /run/keys/test
    #         rmdir $key_upload_dir
    #     EOF
    # '';
}
