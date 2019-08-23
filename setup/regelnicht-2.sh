#!/usr/bin/env bash
#
# Script for setting up our raspberry pi home assistant - part 2

set -Eeu
set -o pipefail

REGELNICHT_IP=192.168.1.40

# TODO somehow make sure the router stops forwarding ssh to the raspberry
# before we boot it from a newly imaged SD;
# otherwise the default password is accessible from the internet until we run
# the commands below!!

# Add our SSH key to the authorized keys using the default password, accepting the host key blindly this first time
# FIXME this won't work when we re-install the raspberry, assuming it will then # generate a new server key;
# maybe remove the host from known_hosts first, since we know we are reinstalling if we run this script.
sshpass -praspberry ssh-copy-id -o StrictHostKeyChecking=accept-new pi@"$REGELNICHT_IP"

# Disable password access since we now have access using our key
ssh pi@"$REGELNICHT_IP" sudo passwd --lock pi

# TODO tune sshd_config (but maybe just pull that from git)

# TODO set up overlayfs against corruption

