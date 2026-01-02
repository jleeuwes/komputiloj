#!/usr/bin/env bash

set -e
set -o pipefail

pandoc -f markdown -t html -s "$1" --metadata title="$1" | w3m -T text/html -no-cookie
