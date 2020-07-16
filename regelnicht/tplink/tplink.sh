#!/usr/bin/env bash

# Before running this script,
# connect the TP-link plug to the network using the Kasa app,
# and assign it a permanent IP in the router config.

set -Eeu -o pipefail

plug=$(basename -- "$0")
case "$plug" in
	wasmachine)
		TPLINK_IP=192.168.1.45
	;;
	*)
		printf "Onbekende plug %q\n" "$plug"
		exit 1
	;;
esac

cd -- "$(dirname -- "$0")"
test -d tplink-smartplug || git clone https://github.com/softScheck/tplink-smartplug.git
cd tplink-smartplug

case "$1" in
	setup)
		echo "Setting cloud URL to something we control to disable the device phoning home."
		echo "Please check that the err_code is 0"
		./tplink_smartplug.py -t "$TPLINK_IP" -j '{"cnCloud":{"set_server_url":{"server":"nonexistent-tplinkcloud.radstand.nl"}}}'
	;;
	*)
		./tplink_smartplug.py -t "$TPLINK_IP" "$@"
	;;
esac

