#!/bin/bash

# Make a backup on regelnicht and move it to ~/backup
# This script assumes known_hosts is set up correctly
# (i.e. you have already logged into regelnicht before)
#
# TODO add logging or something; the script is currently very quiet because it
# is meant to run as a cron job

set -Eeu
set -o pipefail

REGELNICHT_HOST=regelnicht.duckdns.org

mkdir -p ~/backup/incoming

cd ~/backup

snapshot_slug=$(ssh root@$REGELNICHT_HOST hassio snapshots new --name "Automatic backup initiated from radstand" --raw-json | jq --raw-output '.data.slug')
snapshot_file=incoming/"$snapshot_slug".tar

scp -q root@$REGELNICHT_HOST:/backup/"$snapshot_slug".tar "$snapshot_file"

# We do some naive extraction that works because the json is contained
# directly in the tar, not in one of the gzipped files inside.
# It might break down sometimes, which would result in a weird
# filename-with-newlines for the symlink.
snapshot_date=$(egrep --text -o '20[0-9]{2}(-[0-9]{2}){2}T[0-9]{2}(:[0-9]{2}){2}' -- "$snapshot_file" | uniq)
snapshot_dir=$(printf "%s" "$snapshot_date" | sed -E 's/^([0-9]{4})-([0-9]{2})-.*/\1\/\2/')
mkdir -p -- "$snapshot_dir"
snapshot_dated_file="$snapshot_dir"/"$snapshot_date"-"$snapshot_slug".tar
mv -- "$snapshot_file" "$snapshot_dated_file"

ssh root@$REGELNICHT_HOST rm /backup/"$snapshot_slug".tar

