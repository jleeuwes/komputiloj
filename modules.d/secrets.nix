{ boltons, ... }:
with boltons.lib;
{ config, pkgs, lib, ... }:
with lib;
let
  keyOptionsType = types.submodule ({ config, name, ... }: {
    options.name = mkOption {
       example = "secret.txt";
       default = name;
       type = types.str;
       description = ''
         The name of the key file.
       '';
     };

    options.encryptedContent = mkOption {
      type = types.str;
      description = ''
        The encrypted content of the secret.
        Should be encrypted with the age masterkey for this machine.
      '';
    };

    options.user = mkOption {
      default = "root";
      type = types.str;
      description = ''
        The user which will be the owner of the key file.
      '';
    };

    options.group = mkOption {
      default = "root";
      type = types.str;
      description = ''
        The group that will be set for the key file.
      '';
    };

    options.permissions = mkOption {
      default = "0600";
      example = "0640";
      type = types.str;
      description = ''
        The default permissions to set for the key file, needs to be in the
        format accepted by ``chmod(1)``.
      '';
    };
  });
in {

    options = {
        secrets = mkOption {
            default = {};
            type = types.attrsOf keyOptionsType;
        };
    };

    config = {

        systemd.paths.masterkey = {
            pathConfig = {
                PathExists = "/run/keys/masterkey";
            };
            # wantedBy = [ "multi-user.target" ];
        };

        systemd.services = {
            masterkey = {
                serviceConfig = {
                    Type = "simple";
                };
                after = [ "masterkey.path" ];
                # requisite = [ "masterkey.path" ];
                path = [ pkgs.inotify-tools ];
                script = ''
                    inotifywait -qq -e delete_self -e move_self /run/keys/masterkey &

                    if [[ ! -e /run/keys/masterkey ]]; then
                      echo masterkey flapped
                      exit 0
                    fi
                    echo masterkey is present
                    wait %1
                    echo masterkey is gone
                '';
            };

            probeerseltje = {
                wantedBy = [ "multi-user.target" ];
                requires = [ "secret-luks-storage-key.service" ];
                after = [ "secret-luks-storage-key.service" ];
                serviceConfig = {
                    Type = "oneshot";
                    RemainAfterExit = true;
                };
                script = ''
                    echo GOED GEDAAN
                '';
            };
        } // mapNames (name: "secret-${name}") (mapValues (secret: {
            requires = [ "masterkey.service" ];
            after = [ "masterkey.service" ];
            serviceConfig = {
                Type = "simple";
            };
            path = [ pkgs.inotify-tools ];
            preStart = ''
                keyfile=/run/keys/${escapeShellArg secret.name}
                rm -f -- "$keyfile".new
                set -o noclobber
                umask u=rw,go=
                ${pkgs.age}/bin/age --decrypt \
                    -i /run/keys/masterkey \
                    ${pkgs.writeText secret.name secret.encryptedContent} \
                    > "$keyfile".new
                chown -- ${escapeShellArg secret.user}:${escapeShellArg secret.group} "$keyfile".new
                chmod -- ${escapeShellArg secret.permissions} "$keyfile".new
                mv -T -- "$keyfile".new "$keyfile"
            '';
            script = ''
                keyfile=/run/keys/${escapeShellArg secret.name}
                
                inotifywait -qq -e delete_self -e move_self -- "$keyfile" &

                if [[ ! -e $keyfile ]]; then
                  printf '%s disappeared immediately\n' "$keyfile"
                  exit 0
                fi
                printf '%s is present\n' "$keyfile"
                wait %1
                printf '%s is gone\n' "$keyfile"
            '';
        }) config.secrets);
    };

}
