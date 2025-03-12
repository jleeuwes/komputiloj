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
    managed_linux_users = config: filter (user: user.passwordManagedBySleutel) (attrValues (named config.users.users));
in rec {
    packages = {
        sleutel-cli = nixpkgsCurrent.lib.callPackageWith nixpkgsCurrent.packages (import ./sleutel-cli) {
            inherit boltons;
        };
    };

    modules = rec {
        all_in_one = {
            imports = [
                custom-options
                service
                users
            ];
        };
        custom-options = { lib, ... }:
            with lib;
            let userOptions = userArgs: let
                user = userArgs.config;
            in {
                options.passwordManagedBySleutel = mkOption {
                    type = types.bool;
                    default = false;
                    description = lib.mkDoc ''
                        Whether this users password is managed by sleutel,
                        instead of by NixOS directly.
                    '';
                };

                config.hashedPasswordFile = mkIf user.passwordManagedBySleutel "/var/lib/sleutel/shadow/users/${user.name}";
            };
            in {
                options.users.users = mkOption {
                    type = types.attrsOf (types.submodule userOptions);
                };
            };
        service = { pkgs, config, ... }: {
            systemd.services.sleutel-process-passwords = {
                needsStorageVolume = "requires";
                mailOnFailure = true;
                serviceConfig = {
                    Type = "oneshot";
                    User = "sleutel";
                };
                startAt = "*:0,10,20,30,40,50";

                # handover to service running as root that actually changes the password
                # (until the next nixos activation)
                wants = [ "sleutel-chpasswd.service" ];
                before = [ "sleutel-chpasswd.service" ];

                script = ''
                    mkauthfile() {
                        local authfile
                        local user
                        authfile="$1"
                        shift
                        if [[ -e "$authfile" ]]; then
                            printf 'Refusing to overwrite existing file %q\n' "$authfile" >&2
                            return 1
                        fi
                        touch -- "$authfile" # nginx handles an empty file correctly (noone has access)
                                             # TODO check for radicale, ...
                        for user in "$@"; do
                            userfile=~/users/"$user"/password/password.bcrypt
                            if [[ -e $userfile ]]; then
                                printf '%s:%s\n' "$user" \
                                    "$(head -n1 -- "$userfile")" \
                                    >> "$authfile"
                            else
                                printf 'Not including user %q in %q because of missing bcrypt password.\n' \
                                    "$user" "$authfile" >&2
                            fi
                        done
                        if grep -E ':$' -- "$authfile"; then
                            printf 'Aborting because of broken password file %q\n' "$authfile" >&2
                            return 1
                        fi
                    }

                    mknixosuserfile() (
                        local userfile
                        local user
                        userfile="$1"
                        user="$2"
                        head -n1 ~/users/"$user"/password/password.bcrypt > "$userfile"
                        if [[ ! -s "$userfile" ]]; then
                            printf 'Aborting because of empty password file %q\n' "$userfile" >&2
                            return 1
                        fi
                    )

                    mkdir -p ~/apps
                    
                    # app: notie
                    mkdir --mode=u=rwx,g=rx,o= ~/apps/notie.new
                    ${unlines (map (space:
                        "mkdir ~/apps/notie.new/${escapeShellArg space.name}; " +
                        "mkauthfile ~/apps/notie.new/${escapeShellArg space.name}/auth " +
                        escapeShellArgs (map (user: user.name) space.users))
                        (attrValues notie.spaces))}
                    if [[ -e ~/apps/notie ]]; then
                        mv ~/apps/notie ~/apps/notie.old
                    fi
                    mv ~/apps/notie.new ~/apps/notie
                    rm -rf ~/apps/notie.old
                   
                    # app: knol
                    mkdir --mode=u=rwx,g=rx,o= ~/apps/knol.new
                    mkauthfile ~/apps/knol.new/auth ${escapeShellArgs (map (user: user.name) knol.users)}
                    if [[ -e ~/apps/knol ]]; then
                        mv ~/apps/knol ~/apps/knol.old
                    fi
                    mv ~/apps/knol.new ~/apps/knol
                    rm -rf ~/apps/knol.old
                    
                    # linux shadow file nixos stuff
                    mkdir --mode=u=rwx,g=rx,o= /var/lib/sleutel/shadow.new
                    mkauthfile /var/lib/sleutel/shadow.new/auth ${escapeShellArgs (map (user: user.name) (managed_linux_users config))}
                    mkdir /var/lib/sleutel/shadow.new/users
                    ${unlines (map (user: "mknixosuserfile /var/lib/sleutel/shadow.new/users/${escapeShellArg user.name} ${escapeShellArg user.name}")
                        (managed_linux_users config))}
                    if [[ -e /var/lib/sleutel/shadow ]]; then
                        mv /var/lib/sleutel/shadow /var/lib/sleutel/shadow.old
                    fi
                    mv /var/lib/sleutel/shadow.new /var/lib/sleutel/shadow
                    rm -rf /var/lib/sleutel/shadow.old
                '';
            };
            systemd.services.sleutel-chpasswd = {
                mailOnFailure = true;
                serviceConfig = {
                    Type = "oneshot";
                    User = "root";
                };
                path = [ pkgs.shadow ];
                script = ''
                    mkdir --mode=u=rwx,go= -p /var/lib/sleutel
                    chown sleutel:sleutel /var/lib/sleutel
                    if [[ -e /var/lib/sleutel/shadow/auth ]]; then
                        printf 'Running chpasswd...' >&2
                        chpasswd -e < /var/lib/sleutel/shadow/auth
                    else
                        printf 'Not running chpasswd' >&2
                    fi
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
