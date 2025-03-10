{ boltons, nextcloud, nixpkgsCurrent, all, ... }:
with boltons;
let
    inherit (nixpkgsCurrent.lib.strings) escapeShellArg escapeShellArgs;
    # TODO we shouldn't depend on all apps here.
    # Instead, offer nixos configuration options that can be filled by the apps.
    notie = rec {
        # not necessarily existing spaces, but good enough for generating access lists
        space_names = dedup_strings (concatMap (user: user.apps.notie.spaces or []) (attrValues all.users));
        spaces
            = named (listToAttrs (map
                (name: { name = name; value = { users = users_with_space name; }; })
                space_names));
        users_with_space
            = space: filter (user: elem space (user.apps.notie.spaces or []))
                (attrValues all.users);
    };
    knol = {
        users = filter (user: user.apps.knol.enable or false) (attrValues all.users);
    };
    sleutel_users = filter (user: user.apps.sleutel.enable or false) (attrValues all.users);
in rec {
    packages = {
        sleutel-cli = nixpkgsCurrent.lib.callPackageWith nixpkgsCurrent.packages (import ./sleutel-cli) {
            inherit boltons;
        };
    };

    modules = rec {
        all_in_one = {
            imports = [
                service
                users
            ];
        };
        service = {
            systemd.services.sleutel-process-passwords = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                serviceConfig = {
                    Type = "simple";
                    User = "sleutel";
                };
                startAt = "*:0,10,20,30,40,50";
                script = ''
                    mkauthfile() (
                        local appdir
                        local user
                        appdir="$1"
                        shift
                        mkdir -p ~/apps/"$appdir"
                        cd ~/apps/"$appdir"
                        newfile=auth."$RANDOM"
                        touch -- "$newfile" # nginx handles an empty file correctly (noone has access)
                                            # TODO check for radicale, ...
                        for user in "$@"; do
                            userfile=~/users/"$user"/password/password.bcrypt
                            if [[ -e $userfile ]]; then
                                printf '%s:%s\n' "$user" \
                                    "$(head -n1 -- "$userfile")" \
                                    >> "$newfile"
                            else
                                printf 'Not including user %q for app %q because of missing bcrypt password.\n' \
                                    "$user" "$appdir" >&2
                            fi
                        done
                        if grep -E ':$' -- "$newfile"; then
                            printf 'Refusing to install broken password file %q\n' "$newfile" >&2
                            return 1
                        fi
                        mv -- "$newfile" auth
                    )
                    
                    ${unlines (map (space:
                        "mkauthfile notie/${escapeShellArg space.name} " +
                        escapeShellArgs (map (user: user.name) space.users))
                        (attrValues notie.spaces))}
                    
                    mkauthfile knol ${escapeShellArgs (map (user: user.name) knol.users)}
                '';
            };
        };
        users = { pkgs, ... }: {
            users.users.sleutel = {
                uid = 70007;
                group = "sleutel";
                isSystemUser = true;
                home = "/mnt/storage/live/sleutel/rootdir";
                createHome = false;
                shell = pkgs.bashInteractive;
                openssh.authorizedKeys.keys = let
                    user_keys = user: map (user_key user) (attrValues user.ssh.publicKeys or []);
                    user_key  = user: key: 
                        "command=\"${packages.sleutel-cli}/bin/sleutel ${escapeShellArg user.name}\",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-user-rc,restrict,pty ${key}";
                in concatMap user_keys sleutel_users;
            };
            users.groups.sleutel.gid = 70007;

            users.users.nginx.extraGroups = [ "sleutel" ];
        };
    };
}
