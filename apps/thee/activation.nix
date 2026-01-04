{ boltons, nixos, command-platform, all }:
with boltons;
with nixos.lib.strings;
let
    nixos = nixos;
    inherit (nixos.local.packageBuilders) writeShellApplication;
    inherit (nixos.portable.packageBuilder) writeTextDir symlinkJoin;
    url_base = "https://thee.radstand.nl";
    api_base = "${url_base}/api/v1";

    user2gitea_create = user: toJSON {
        email = user.email;
        full_name = user.fullName;
        login_name = user.name;
        username = user.name;
        must_change_password = user.isHuman;
        visibility = user.apps.thee.visibility;
    };
    user2gitea_update_enabled = user: toJSON {
        email = user.email;
        full_name = user.fullName;
        login_name = user.name;
        username = user.name;
        prohibit_login = false;
        visibility = user.apps.thee.visibility;
    };
    user2gitea_update_disabled = user: toJSON {
        email = user.email;
        full_name = user.fullName;
        login_name = user.name;
        username = user.name;
        prohibit_login = true;
        visibility = "private";
    };

    user2gitea_keys = user: attrValues (mapAttrs key2gitea (user.ssh.publicKeys or {}));
    key2gitea = name: key: toJSON {
        key = key;
        title = "komputiloj_managed:${name}";
        read_only = false;
    };
    
    userlist = attrValues all.users;
    declared_users   = map (user: user.name) userlist;
    users_to_enable  = filter (user: user.apps.thee.enable or false) userlist;
    users_to_disable = filter (user: !(user.apps.thee.enable or false)) userlist;
    keys_per_user    = listToAttrs (map (user: { name = user.name; value = user2gitea_keys user; }) userlist);

    enabled_user_drv = user: symlinkJoin {
        name = user.name;
        paths = [
            (writeTextDir "create.json" (user2gitea_create user))
            (writeTextDir "update.json" (user2gitea_update_enabled user))
            (writeShellApplication {
                name = "generate-password";
                runtimeInputs = [ nixos.local.legacyPackages.pwgen ];
                text = stripTabs ''
                    ${if user.isHuman
                    then "pwgen 10 1"
                    else "pwgen -s 100 1"}
                '';
            })
        ];
    };
    disabled_user_drv = user: symlinkJoin {
        name = user.name;
        paths = [
            (writeTextDir "update.json" (user2gitea_update_disabled user))
        ];
    };
    script_drv = command-platform.local.packageBuilders.writeCommand {
        name = "activate";
        runtimeInputs = [ nixos.local.legacyPackages.jq ];
        text = ''
            cd -- "$KOMPUTILOJ_PATH"
            THEE_USER=jeroen
            THEE_PASSWORD=$(wachtwoord cat -n secrets/jeroen@thee.radstand.nl)

            do_curl_basic() {
                curl \
                    --no-progress-meter \
                    -H "Content-Type: application/json" \
                    --config <(
                        printf -- '--user %q\n' "$THEE_USER:$THEE_PASSWORD"
                    ) "$@"
            }
            do_curl_bearer() {
                curl \
                    --no-progress-meter \
                    -H "Content-Type: application/json" \
                    --config <(
                        printf -- '-H "Authorization: token %q"\n' "$THEE_TOKEN"
                    ) "$@"
            }

            do_curl_basic -X DELETE \
                "${api_base}/users/$THEE_USER/tokens/komputiloj" 1>&2
            THEE_TOKEN=$(do_curl_basic --fail-with-body -X POST \
                "${api_base}/users/$THEE_USER/tokens" \
                --data-raw '{"name": "komputiloj", "scopes": ["write:admin", "write:user"]}' | jq -r .sha1)
            
            declare -a existing_users
            declare -A user_exists
            declare -A user_processed
            # Make sure the array has a value for each declared user:
            for u in ${escapeShellArgs declared_users}; do
                user_exists[$u]=0
                user_processed[$u]=0
            done
            # Now set the value to 1 for each user already existing in gitea:
            userdump=$(do_curl_bearer --fail-with-body "${api_base}/admin/users?limit=50" | jq -c '.[]|.login')
            while IFS= read -r u_json; do
                u=$(printf '%s' "$u_json" | jq -r) # convert to raw string
                user_exists[$u]=1
                user_processed[$u]=0
                existing_users+=( "$u" )
            done <<< "$userdump"

            # We currently completely ignore paging.
            # If the number of users gets bigger than MAX_RESPONSE_ITEMS
            # (see https://docs.gitea.io/en-us/administration/config-cheat-sheet/#api-api)
            # we will SILENTLY miss any further pages.
            # Here are some checks to catch that,
            # assuming default values for MAX_RESPONSE_ITEMS and DEFAULT_PAGING_NUM
            # TODO We should use something like https://github.com/Langenfeld/py-gitea
            if [[ ${"\${#existing_users[@]}"} -ge 30 ]]; then
                echo >&2
                echo "! DANGER!!!" >&2
                echo "! We are getting close to the maximum number of users supported by this script." >&2
                echo >&2
            elif [[ ${"\${#existing_users[@]}"} -ge 50 ]]; then
                echo "! ABORT!!!" >&2
                echo "! We have reached the maximum number of users supported by this script." >&2
                exit 1
            fi

            enable_user() {
                local user
                local infodir
                user="$1"
                infodir="$2"
                echo >&2
                if [[ ${"\${user_exists[$user]}"} -eq 1 ]]; then
                    echo "User $user exists, should enable -> UPDATE" >&2
                    do_curl_bearer --fail-with-body -X PATCH \
                        "${api_base}/admin/users/$user" \
                        -d @"$infodir"/update.json
                else
                    echo "User $user does not exist, should enable -> CREATE" >&2
                    (
                        USER_PASSWORD=$("$infodir"/bin/generate-password) \
                        jq '. += {password: $ENV.USER_PASSWORD}' \
                    |
                        do_curl_bearer --fail-with-body -X POST \
                            "${api_base}/admin/users" \
                            -d @-
                    ) < "$infodir"/create.json
                fi
                user_processed[$user]=1
            }

            disable_user() {
                local user
                local infodir
                user="$1"
                infodir="$2"
                echo >&2
                if [[ ${"\${user_exists[$user]}"} -eq 1 ]]; then
                    echo "User $user exists, should disable -> UPDATE" >&2
                    do_curl_bearer --fail-with-body -X PATCH \
                        "${api_base}/admin/users/$user" \
                        -d @"$infodir"/update.json
                else
                    echo "User $user does not exist, should disable -> NO-OP" >&2
                fi
                user_processed[$user]=1
            }
            
            delete_user_keys() {
                local user
                user="$1"
                local key_title
                local key_id
                local key_json
                local keydump
                keydump=$(do_curl_bearer --fail-with-body "${api_base}/users/$user/keys?limit=50" | jq -c '.[]')
                while IFS= read -r key_json; do
                    key_title=$(printf '%s' "$key_json" | jq -r .title)
                    key_id=$(printf '%s' "$key_json" | jq -r .id)
                    if [[ "$key_title" = komputiloj_managed:* ]]; then
                        echo "Deleting managed key for user $user: $key_title" >&2
                        do_curl_bearer --fail-with-body -X DELETE \
                            "${api_base}/admin/users/$user/keys/$key_id"
                    else
                        echo "Ignoring non-managed key for user $user: $key_title" >&2
                    fi
                done <<< "$keydump"
            }
            add_user_keys() {
                local user
                local json
                user="$1"
                shift
                while [[ $# -gt 0 ]]; do
                    json="$1"
                    shift
                    
                    echo -n "Adding key for user $user: " >&2
                    printf '%s' "$json" | jq -r .title >&2
                    do_curl_bearer --fail-with-body -X POST \
                        "${api_base}/admin/users/$user/keys" \
                        --data-raw "$json"
                done
            }
            update_user_keys() {
                # Naive implementation: delete all managed keys, then re-add.
                # Of course this means that there is a short window in which the user loses access.
                echo >&2
                echo "Updating keys for user $1" >&2
                delete_user_keys "$1"
                add_user_keys "$@"
            }
            
            ${unlines (map
                (user: "enable_user ${escapeShellArg user.name} ${enabled_user_drv user}")
                users_to_enable)}
            ${unlines (map
                (user: "disable_user ${escapeShellArg user.name} ${disabled_user_drv user}")
                users_to_disable)}

            for user in "${"\${existing_users[@]}"}"; do
                if [[ ${"\${user_processed[$user]}"} -ne 1 ]]; then
                    echo "User $user exists but is not declared -> NO-OP" >&2
                fi
            done

            ${unlines (attrValues (mapAttrs
                (name: value: "update_user_keys ${escapeShellArg name} ${escapeShellArgs value}")
                keys_per_user))}
        '';
    };
in script_drv
