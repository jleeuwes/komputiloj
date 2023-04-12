{ lib, writeShellApplication, writeTextDir, symlinkJoin, jq, pwgen, users, ... }:
with import ../../utilecoj.nix;
with builtins;
with lib.strings;
let
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
    
    userlist = attrValues users;
    declared_users   = map (user: user.name) userlist;
    users_to_enable  = filter (user: user.apps.thee.enable or false) userlist;
    users_to_disable = filter (user: !(user.apps.thee.enable or false)) userlist;

    enabled_user_drv = user: symlinkJoin {
        name = user.name;
        paths = [
            (writeTextDir "create.json" (user2gitea_create user))
            (writeTextDir "update.json" (user2gitea_update_enabled user))
            (writeShellApplication {
                name = "generate-password";
                runtimeInputs = [ pwgen ];
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
    script_drv = writeShellApplication {
        name = "activate";
        runtimeInputs = [ jq ];
        text = ''
            [[ -n "$THEE_USER" ]]
            [[ -n "$THEE_PASSWORD" ]]
            set -u
            do_curl_basic() {
                curl \
                    -H "Content-Type: application/json" \
                    --config <(
                        printf -- '--user %q\n' "$THEE_USER:$THEE_PASSWORD"
                    ) "$@"
            }
            do_curl_bearer() {
                curl \
                    -H "Content-Type: application/json" \
                    --config <(
                        printf -- '-H "Authorization: token %q"\n' "$THEE_TOKEN"
                    ) "$@"
            }

            do_curl_basic -X DELETE \
                "${api_base}/users/$THEE_USER/tokens/komputiloj" 1>&2
            THEE_TOKEN=$(do_curl_basic --fail -X POST \
                "${api_base}/users/$THEE_USER/tokens" \
                --data-raw '{"name": "komputiloj"}' | jq -r .sha1)
            
            declare -a existing_users
            declare -A user_exists
            declare -A user_processed
            # Make sure the array has a value for each declared user:
            for u in ${escapeShellArgs declared_users}; do
                user_exists[$u]=0
                user_processed[$u]=0
            done
            # Now set the value to 1 for each user already existing in gitea:
            while IFS= read -r u_json; do
                u=$(printf '%s' "$u_json" | jq -r) # convert to raw string
                user_exists[$u]=1
                user_processed[$u]=0
                existing_users+=( "$u" )
            done < <(
                do_curl_bearer "${api_base}/admin/users" | jq -c '.[]|.login'
            )

            enable_user() {
                local user
                local infodir
                user="$1"
                infodir="$2"
                if [[ ${"\${user_exists[$user]}"} -eq 1 ]]; then
                    echo "User $user exists, should enable -> UPDATE" >&2
                    do_curl_bearer --fail -X PATCH \
                        "${api_base}/admin/users/$user" \
                        -d @"$infodir"/update.json
                else
                    echo "User $user does not exist, should enable -> CREATE" >&2
                    (
                        USER_PASSWORD=$("$infodir"/bin/generate-password) \
                        jq '. += {password: $ENV.USER_PASSWORD}' \
                    |
                        do_curl_bearer --fail -X POST \
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
                if [[ ${"\${user_exists[$user]}"} -eq 1 ]]; then
                    echo "User $user exists, should disable -> UPDATE" >&2
                    do_curl_bearer --fail -X PATCH \
                        "${api_base}/admin/users/$user" \
                        -d @"$infodir"/update.json
                else
                    echo "User $user does not exist, should disable -> NO-OP" >&2
                fi
                user_processed[$user]=1
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
        '';
    };
in script_drv
