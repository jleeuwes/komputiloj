#!/usr/bin/env bash

# Script for setting up things,
# to be run on the raspberry pi once after a (re-)install.
# Run it as root.
# Use first_contact.sh from another machine to get this script on regelnicht.

set -Eeu
set -o pipefail

if test "$(id -u)" != 0; then
	echo "Please run this script as root."
	exit 1
fi

echo "Stopping home assistant..."
systemctl stop home-assistant@homeassistant.service

##### Strategic replacements in /etc

cd /etc
if test ! -d komputiloj; then
	echo "komputiloj is not in the expected location (how am I running?!)"
	exit 1
fi

echo "Setting up mounts..."
rm -f fstab.bak
cp -a --dereference fstab fstab.bak
ln -sf komputiloj/regelnicht/etc/fstab
mkdir /mnt/data
mount -a # mount the new config

echo "Setting up some other system things..."
ln -sf komputiloj/regelnicht/etc/localtime

##### Tactical operations around the home directories

echo "Moving home directories to data partition..."
mv /home /mnt/data/home
ln -s /mnt/data/home /home

echo "Moving homeassistant config from komputiloj in place..."
cd ~homeassistant
cp -a .homeassistant{,.bak}
cd .homeassistant
# Make shallow symlinks to everything in komputiloj
# (Note: hidden files won't be linked; directories will be symlinked as a whole)
ln -sf /etc/komputiloj/regelnicht/home/homeassistant/.homeassistant/* .

##### Upgrades

echo "Upgrading the OS..."
apt-get update --allow-releaseinfo-change
apt-get upgrade -y

echo "Upgrading home assistant..."
hassbian-config upgrade homeassistant

##### Finalization

echo "Starting home assistant..."
systemctl start home-assistant@homeassistant.service

echo "That's it, I'm done!"
echo "The latest configuration from komputiloj is activated."
echo "You can now change things, via the welcome screen or in the config files."
echo "Please don't forget to commit your changes!"
# TODO also move some other fast-changing stuff to the /mnt/data
# TODO make a sync script for pulling komputiloj
# TODO pull in datumoj so we can make backups of the homeassistant database
# TODO see what else of /etc we should put in kompituloj
# TODO set up overlayfs against corruption?

