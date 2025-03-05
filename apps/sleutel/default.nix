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

                    update_gitea_user() {
                        local user
                        local password_hash
                        local algo
                        user="$0"
                        if ! printf '%s' "$user" | grep -E '^[a-z0-9_-]+$' > /dev/null; then
                            printf 'Refusing to work with username %q\n' "$user" >&2
                            return 1
                        fi
                        password_hash=$(head -n1 -- ~/users/"$user"/password/password.bcrypt)
                        if ! printf '%s' "$password_hash" | grep -E '^[A-Za-z0-9/.$]+$' > /dev/null; then
                            printf 'Refusing to work with password hash of user %q\n' "$user" >&2
                            return 1
                        fi
                        algo=(grep -Eo '^\$2[ab]\$[0-9]+' | sed -E 's/\$2[ab]$/bcrypt$/')
                        
                        sqlite3 /mnt/storage/live/gitea/rootdir/data/gitea.db \
                            "update user set passwd='$password_hash', passwd_hash_algo='$algo', must_change_password=0 where name='$user'"
                    }
                    
                    ${unlines (map (space:
                        "mkauthfile notie/${escapeShellArg space.name} " +
                        escapeShellArgs (map (user: user.name) space.users))
                        (attrValues notie.spaces))}
                    
                    mkauthfile knol ${escapeShellArgs (map (user: user.name) knol.users)}
                    
                    for user in ${escapeShellArgs (map (user: user.name) thee.users)}; do
                        update_gitea_user "$user"
                    done
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
