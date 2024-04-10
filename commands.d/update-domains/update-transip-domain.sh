export TRANSIP_URL=https://api.transip.nl/v6
export TIPCTL_CFG=~/.config/transip-api/cli-config.json

get_access_token() {
	jq_script=$(cat <<-'EOF'
	{
		apiUrl: $ENV.TRANSIP_URL,
		loginName: $ENV.TRANSIP_USER,
		apiPrivateKey: $ENV.TRANSIP_PRIVKEY,
		apiUseWhitelist: false,
		showConfigFilePermissionWarning: true
	}
	EOF
	)
	
	touch "$TIPCTL_CFG"
	chmod go-r "$TIPCTL_CFG"
	jq -n "$jq_script" > "$TIPCTL_CFG"
	cleanup_config() {
		rm -f "$TIPCTL_CFG"
	}
	trap cleanup_config EXIT
	
	tipctl apiutil:requestaccesstoken 2>/dev/null
	
	cleanup_config
}

get_access_token_without_tipctl() {
	# Cannot get this to work yet.
	
	export NONCE="$SRANDOM"
	auth_payload=$(jq -nc '{ login: $ENV.TRANSIP_USER, nonce: $ENV.NONCE }')
	binary_signature=$(cat <<<"$auth_payload" | openssl dgst -sha512 -sign <(cat <<<"$TRANSIP_PRIVKEY"))
	signature=$(base64 -w0 <(cat <<<"$binary_signature"))
	
	auth_payload={}
	
	curl -X POST \
		-H "Content-Type: application/json" \
		--config <(
			printf -- '-H "Signature: %s"\n' "$signature"
		) \
		--fail \
		--data-raw "$auth_payload" \
		"$TRANSIP_URL"/auth
}

# TODO: why do this per domain?
TRANSIP_TOKEN=$(get_access_token)

domain=$1

payload=$(cat -- "$2" | \
	# remove comment lines:
	grep -Ev '^;' | \
 	# remove empty lines:
 	grep -Ev '^[[:space:]]*$' | \
 	# make jsons:
 	jq -Rc 'split("\t+"; "") | { name: .[0], expire: .[1], type: .[3], content: .[4] }' | \
 	# join json:
 	jq --slurp -c '{dnsEntries: .}')

curl -X PUT \
	-H "Content-Type: application/json" \
	--config <(
		printf -- '-H "Authorization: Bearer %s"\n' "$TRANSIP_TOKEN"
	) \
	--fail \
	--data-raw "$payload" \
	"$TRANSIP_URL"/domains/"$domain"/dns
