#!/usr/bin/env bash
#
# Script for setting up our raspberry pi home assistant

# We use hassbian (so we can put some things on the pi besides home assistant).
# Based on setup instructions at https://www.home-assistant.io/docs/installation/hassbian/installation/

set -Eeu
set -o pipefail

SDCARD_DEVICE=/dev/mmcblk0

IMAGE_DOWNLOAD_URL="https://github.com/home-assistant/pi-gen/releases/download/v1.6.1/image_2019-07-02-Hassbian.zip"
IMAGE_DOWNLOAD_FILENAME=$(basename -- "$IMAGE_DOWNLOAD_URL")
# Hash determined after first download; we're banking on github to have given us the correct file,
# but the next time we use this hash to make sure we have the same file.
IMAGE_DOWNLOAD_HASH="dd3a73c57f8032c7e53c50fd60002f234048d3567e7850748465faa66ad9c02be3292e65d670b311ca747d484a1f546ec8b32c69d2e9b76ec2911904957ccbd4"
IMAGE_FILENAME=2019-07-02-Hassbian.img

give_curl_command() {
	printf "  curl -L -o %q -- %q\n" "$IMAGE_DOWNLOAD_FILENAME" "$IMAGE_DOWNLOAD_URL"
}

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

WORK_DIR=$(mktemp -d)

unzip -d "$WORK_DIR" -- "$IMAGE_DOWNLOAD_FILENAME"

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

echo ":)  :)  :)  :)"
echo "Thanks! You can now OVERWRITE the SD card's content:"
printf "  sudo dd if=%q of=%q\n" "$WORK_DIR"/"$IMAGE_FILENAME" "$SDCARD_DEVICE"
echo "Then clean up the temporary dir:"
printf "  rm -rf -- %q\n" "$WORK_DIR"
echo "Then boot the raspberry with the SD card and we'll continue."
echo "TODO followup command"

