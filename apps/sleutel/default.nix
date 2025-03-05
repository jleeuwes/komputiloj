{ boltons, nextcloud, nixpkgsCurrent, all, ... }:
with boltons;
let
    inherit (nixpkgsCurrent.lib.strings) escapeShellArg escapeShellArgs;
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
in {
    packages = {
        sleutel-cli = nixpkgsCurrent.lib.callPackageWith nixpkgsCurrent.packages (import ./sleutel-cli) {};
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
        users = {
            users.users.sleutel = {
                uid = 70007;
                group = "sleutel";
                isSystemUser = true;
                home = "/mnt/storage/live/sleutel/rootdir";
                createHome = false;
            };
            users.groups.sleutel.gid = 70007;

            users.users.nginx.extraGroups = [ "sleutel" ];
        };
    };
}
