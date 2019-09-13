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

perform_upgrades() {
	echo "Updating the machine..."
	ssh pi@"$REGELNICHT_IP" sudo apt-get update --allow-releaseinfo-change
	ssh pi@"$REGELNICHT_IP" sudo apt-get upgrade -y
}

# TODO tune sshd_config (but maybe just pull that from git)

# TODO set up overlayfs against corruption?

set_up_komputiloj() {
	# idee: script in komputiloj zetten, als eerste git clonen en dan het script uitvoeren,
	# in plaats van steeds stukjes via ssh doen
	
	ssh pi@"$REGELNICHT_IP" /usr/bin/bash <<EOF
		set -Eeu
		set -o pipefail
		git clone https://github.com/jleeuwes/komputiloj.git
		sudo -u homeassistant mv ~homeassistant/.homeassistant{,.old}
		sudo -u homeassistant ln -s ~pi/komputiloj/regelnicht/home/homeassistant/.homeassistant ~homeassistant
EOF

	# TODO scripten of via /etc-beheer regelen
	# Over dat laatste: best met een schone install beginnen, dat inchecken, dan tweaken
	# sudo ln -sf /usr/share/zoneinfo/Europe/Brussels /etc/localtime
}

trust_host_key
# perform_upgrades
# set_up_komputiloj

