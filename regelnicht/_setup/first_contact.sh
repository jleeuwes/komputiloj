#!/usr/bin/env bash
#
# Script for establishing first contact with our raspberry pi home assistant
# This script does some things you only want to do after (re-)install,
# not during routine operation!

set -Eeu
set -o pipefail

REGELNICHT_IP=192.168.1.40

trust_host_key() { # tijdelijk even uitgezet
	# Removal is necessary assuming the raspberry will generate a new server key on
	# re-install. Obviously do not do this in routine operation!
	echo "Removing old host key(s) from known_hosts file..."
	ssh-keygen -R "$REGELNICHT_IP"
	echo "Accepting the raspberry's host key..."
	# Add our SSH key to the authorized keys using the default password, accepting the host key blindly this first time
	ssh -o StrictHostKeyChecking=accept-new pi@"$REGELNICHT_IP" echo "First contact!"
}

set_up_komputiloj() {
	echo "Checking out kompituloj on regelnicht..."
	ssh pi@"$REGELNICHT_IP" /usr/bin/bash <<EOF
		set -Eeu
		set -o pipefail
		sudo git clone https://github.com/jleeuwes/komputiloj.git /etc/komputiloj
EOF
}

trust_host_key
set_up_komputiloj

echo "Things are set up on regelnicht."
echo "Please log in on regelnicht and run:"
echo "    sudo apt-get install screen"
echo "    sudo screen"
echo "    /etc/komputiloj/regelnicht/_setup/first_steps.sh"
echo "(The last command being inside the screen session)"

