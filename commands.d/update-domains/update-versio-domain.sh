# NOTE: the testapi url as specified on https://www.versio.nl/RESTapidoc/ gives a 404.
# api_base=https://www2.versio.nl/testapi/v1
api_base=https://www2.versio.nl/api/v1

domain=$1

payload=$(cat -- "$2" | \
	# remove comment lines:
	grep -Ev '^;' | \
 	# remove empty lines:
 	grep -Ev '^[[:space:]]*$' | \
 	# make jsons:
 	jq -Rc 'split("\t+"; "") | { name: .[0], ttl: .[1] | tonumber, type: .[3] }
		+ if .[3] == "MX"
		  then (.[4] | split(" +"; "") | { prio: .[0] | tonumber, value: .[1] })
		  else { prio: 0, value: .[4] }
		  end' | \
 	# join json:
 	jq --slurp -c '{dns_records: .}')

curl -X POST \
	-H "Content-Type: application/json" \
	--config <(
		printf -- '--user "%s"\n' "$VERSIO_USER"
	) \
	--fail \
	-d "$payload" \
	"$api_base"/domains/"$domain"/update
echo
