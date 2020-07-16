#!/usr/bin/env bash
#
# This is the script that prepares an SD card from scratch.
# This can mean two things:
#
# 1. We start from a clean Hassbian image, configure it manually and record that
#    configuration somehow (in scripts and/or in git).
# 2. We start from a clean Hassbian image en auto-configure that to restore our
#    recorded configuration.
#
# This first version(s) of this script will attempt 1,
# then the script will probably evolve to be able to perform 2 regularly.
# It's not clear if 1 is still needed then, except for starting over.
#
# Note that the idea is that we also take steps to *prevent* SD card corruption,
# so ideally this script won't be needed most of the time.


set -Eeu
set -o pipefail

SDCARD_DEVICE=/dev/mmcblk0
# This much will be reserved for OS; the rest will be available for data
# MAKE SURE this is a valid parted/fdisk size specifier without funky characters
# because we will be putting it in our commands without checking!
SDCARD_PRIMARY_PARTITION_SIZE=15GB

IMAGE_DOWNLOAD_URL="https://github.com/home-assistant/pi-gen/releases/download/v1.6.1/image_2019-07-02-Hassbian.zip"
IMAGE_DOWNLOAD_FILENAME=$(basename -- "$IMAGE_DOWNLOAD_URL")
# Hash determined after first download; we're banking on github to have given us the correct file,
# but the next time we use this hash to make sure we have the same file.
IMAGE_DOWNLOAD_HASH="dd3a73c57f8032c7e53c50fd60002f234048d3567e7850748465faa66ad9c02be3292e65d670b311ca747d484a1f546ec8b32c69d2e9b76ec2911904957ccbd4"

give_curl_command() {
	printf "  curl -L -o %q -- %q\n" "$IMAGE_DOWNLOAD_FILENAME" "$IMAGE_DOWNLOAD_URL"
}

await_confirmation() {
	read -p "May I continue? Type y or yes: " CONFIRM
	if test "$CONFIRM" != y -a "$CONFIRM" != yes; then
		echo "You didn't say yes, so I'll stop!"
		exit 2
	fi
}

check_download() {
	if test ! -f "$IMAGE_DOWNLOAD_FILENAME"; then
		echo "I see no file called $IMAGE_DOWNLOAD_FILENAME"
		echo "Please make sure it is in the current working directory, for example by running:"
		give_curl_command
		exit 1
	fi
	
	echo "Checking integrity of downloaded file $IMAGE_DOWNLOAD_FILENAME..."
	if ! printf "%s  %s\n" "$IMAGE_DOWNLOAD_HASH" "$IMAGE_DOWNLOAD_FILENAME" | sha512sum -c; then
		echo "!!!!! File is corrupted or manipulated by an evil adversary. Please download again:"
		give_curl_command
		exit 1
	fi
}

check_sdcard_device_available() {
	# We do some detection stuff here to make sure we have the right device.
	echo
	if test -b "$SDCARD_DEVICE"; then
		echo "An SD card seems to be inserted already. Please remove it."
		while test -b "$SDCARD_DEVICE"; do
			sleep 1
		done
	fi
	echo "Please insert the SD card that I can write to."
	while test ! -b "$SDCARD_DEVICE"; do
		sleep 1
	done
}

flash_sdcard() {
	echo "SUDO Asking root to give you ownership over $SDCARD_DEVICE..."
	echo "(This will stick until you remove the SD card)"
	# Note: we tried chowning partition devices as well later in the script,
	# but those ownerships seem to be reset by the different
	# parted/e2fsck/resize2fs commands so that's not a valid strategy.
	sudo chown "$USER" "$SDCARD_DEVICE"
	
	echo "Going to write the image to the SD card..."
	echo "SD CARD $SDCARD_DEVICE WILL BE OVERWRITTEN COMPLETELY!"
	await_confirmation
	echo "Please wait while I write the image..."
	unzip -p -- "$IMAGE_DOWNLOAD_FILENAME" > "$SDCARD_DEVICE"
}

repartition_sdcard() {
	echo "SUDO Partition time! I will:"
	echo "  - expand the primary partition to $SDCARD_PRIMARY_PARTITION_SIZE to accomodate later OS updates"
	echo "  - fill the rest of the SD card with a data partition"
	# Warning: the PARTUUID (= dos/windows disk label-id) will change if we use parted, breaking our boot cmdline and fstab!
	#   see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=35714 and the links to raspberry's workarounds
	# So, first store the current label-id ( thanks to https://unix.stackexchange.com/a/419116 )
	disk_id_bytes=$(dd if="$SDCARD_DEVICE" bs=1 count=4 skip=440)
	# Then run parted (it needs root to inform the kernel of changes)
	sudo parted "$SDCARD_DEVICE" -- \
		resizepart 2 "$SDCARD_PRIMARY_PARTITION_SIZE" \
		mkpart primary ext4 "$SDCARD_PRIMARY_PARTITION_SIZE" -1s
	# Then put back the label-id:
	printf "%s" "$disk_id_bytes" | sudo dd of="$SDCARD_DEVICE" bs=1 count=4 seek=440
	
	echo "Fixing/resizing/creating filesystems..."
	# These commands also seem to reset partition device ownership, so we just use sudo here as well
	sudo e2fsck -f "${SDCARD_DEVICE}p2"
	sudo resize2fs "${SDCARD_DEVICE}p2"
	# Apparently the filesystem can survive all the above, so first destroy it to prevent nagging by mkfs:
	sudo dd if=/dev/zero of="${SDCARD_DEVICE}p3" count=10
	# (Alternatively, maybe it's a good idea to keep the data fs intact?)
	sudo mkfs.ext4 "${SDCARD_DEVICE}p3"
}

preconfigure_system() {
	local MEDIA_DIR

	# echo "Mounting the SD card boot partition..."
	# pmount -- "${SDCARD_DEVICE}p1"
	# MEDIA_DIR=/media/$(basename -- "${SDCARD_DEVICE}p1")
	# echo "TODO remove init=/usr/lib/raspi-config/init_resize.sh from cmdline.txt to prevent first-time-boot modal dialog complaining about not being able to resize fs (due to our custom partitioning"
	# echo "Umounting the SD card boot partition..."
	# pumount -- "${SDCARD_DEVICE}p1"

	echo "Mounting the SD card primary partition..."
	pmount -- "${SDCARD_DEVICE}p2"
	MEDIA_DIR=/media/$(basename -- "${SDCARD_DEVICE}p2")
	echo "SUDO Disabling pi user password..."
	# It's important to do this before booting the raspberry,
	# because otherwise it will be accessible with the default password,
	# which is NOT GOOD if this is a reinstall and the router is configured to
	# forward SSH from the internet to the raspberry.
	sudo sed -Ei 's/^pi:[^:]+:/pi:*:/' "$MEDIA_DIR"/etc/shadow
	echo "Putting your SSH key in place..."
	# This is a hack relying on the coincidence that we have the same UID as the pi user.
	# The nice thing is: we don't have to check this, because if this would happen to not be true,
	# the next commands would simply fail with a permission error.
	mkdir "$MEDIA_DIR"/home/pi/.ssh
	cat ~/.ssh/id_rsa.pub > "$MEDIA_DIR"/home/pi/.ssh/authorized_keys
	echo "Unmounting the SD card primary partition..."
	pumount -- "${SDCARD_DEVICE}p2"
}

check_download
check_sdcard_device_available
flash_sdcard
repartition_sdcard
preconfigure_system

# TODO prepare overlayfs like https://github.com/JasperE84/root-ro but with a real partition instead of tmpfs,
# for persistence with easy reset (just nuke the data partition)

echo
echo "We're done!"
echo "Go ahead and put the SD card in the raspberry and boot it up!"
echo "Let it do its business of installing itself."
echo "When http://192.168.1.40:8123/ gives a welcome screen,"
echo "Run first_contact.sh and follow the instructions."

echo "And then there will be some further steps but TODO YOLO"

