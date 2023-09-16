{ boltons, komputiloj, nixpkgsCurrent, ... }:
with boltons;
rec {
    makeMachineDeployable = machine: machine // {
        deploymentPackage = makeMachineDeploymentPackage machine;
    };
    makeMachineDeploymentPackage = machine: nixpkgsCurrent.lib.writeShellApplication {
        name = "deploy";
        runtimeInputs = [
            nixpkgsCurrent.packages.openssh
            komputiloj.packages.wachtwoord
        ];
        text = ''
            [[ -n "$KOMPUTILOJ_PATH" ]]
            set -u

            upload_key() {
                local local_name="$1"
                local remote_file="$2"
                local remote_command
                remote_command=$(printf 'cat > %q' "$remote_file")
                # TODO move secrets out of serviloj
                # shellcheck disable=SC2029
                wachtwoord cat -n \
                    "$KOMPUTILOJ_PATH"/serviloj/secrets/"$local_name" | \
                    ssh root@${machine.hostName} "$remote_command"
            }

            key_upload_dir=/run/keys/upload."$RANDOM"
            # shellcheck disable=SC2087
            ssh root@${machine.hostName} bash - <<-EOF
                mkdir $key_upload_dir
                chmod go= $key_upload_dir
            EOF

            upload_key luks-storage@hetzner "$key_upload_dir"/luks-storage

            # shellcheck disable=SC2087
            ssh root@${machine.hostName} bash - <<-EOF
                mv $key_upload_dir/* /run/keys/test
                rmdir $key_upload_dir
            EOF
        '';
    };
}
