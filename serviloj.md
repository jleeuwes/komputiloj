# Serviloj

Work in progress to set up a more
[cattle-like](https://medium.com/@Joachim8675309/devops-concepts-pets-vs-cattle-2380b5aab313)
(set of) server(s) for running my online services.

## Storage

### Design

We want some permanent storage, of course,
but it should be separated from the OS and unimportant data.
To achieve this we put permanent data on a Hetzner volume.
At any time, we should be able to
detach the volume from the VPS,
delete and recreate the VPS,
attach the volume
and have a working system again.
Nothing of value should be lost.

We use a BTRFS filesystem
on top of LUKS
on top of the Hetzner volume.

#### subvolumes

We have these subvolumes:

- `live` holds a subvolume per app/service, holding persistent data for day-to-day use
	- `live/komputiloj` holds data pertaining to komputiloj as a whole.
	  We could use it, for instance, for logs that we want to really keep.
- `work` holds temporary data.
  Put stuff here instead of in e.g. `/tmp` to make sure it is not stored
  unencrypted.
  Put stuff here instead of under `live` if it does not need to be snapshot and
  backed up.
- `backups` holds backup data from other systems
- `snapshots` holds read-only snapshots of volumes under `live`
- `archives` holds read-only subvolumes that are not in active use

Some notes:

- Make sure ALL subvolumes are owned by root and no regular user has write permission on them.
  Otherwise, read-only subvolume snapshots are not safe against modification,
  because the owner, which the snapshot inherits, can make such snapshots writeable.
- If some app/service needs to own its root folder, create a `rootdir` inside
  the subvolume with the non-root owner, so the subvolume can be owned by root.
- We make regular automatic snapshots of the `live` volume under the `snapshots` volume.
- We make **irregular** backups from the `snapshots` volume to an external system (my laptop).
- It doesn't make sense to place backups of our server under `backups` - this is what `snapshots` is for.
- It doesn't make sense to make snapshots of stuff under `backups`,
  because then files from external systems will end up in a loop
  external system > `backups` > `snapshots` > external system
- We might also want a subvolume holding temporary subvolumes for working on some data?
  We might also consider such work a service and put it under `live`.

### One time setup

I used a part of the instructions on
<https://binfalse.de/2018/11/28/mount-multiple-subvolumes-of-a-luks-encrypted-btrfs-through-pam-mount/>
and created the filesystem as follows
from a clean install NixOS VPS.

**Only run the `nix-env` commands if you intend to recreate the VPS afterwards!**
We don't want to end up with an imperatively modified system.

Create the LUKS partition:

	nix-env iA nixos.cryptsetup
	cryptsetup --verify-passphrase -v --cipher aes-xts-plain64 --key-size 256 luksFormat /dev/sdb

The cipher and key size were chosen based on the results of
`cryptsetup benchmark`.

I created a random password and put that in my password store.
Using `--verify-passphrase` I pasted the password once from my store,
and typed in in for verification,
to make sure I have the correct password set.

Decrypt and map to `/dev/mapper/storage`:

	cryptsetup luksOpen UUID=6c8d5be7-ae46-4e51-a270-fd5bdce46f3b storage

The UUID can be obtained by running `cryptsetup luksDump /dev/sdb`.

Now create the BTRFS filesystem:

	nix-env -iA nixos.btrfs-progs
	mkfs.btrfs /dev/mapper/storage

Now mount the filesystem:

	mkdir -p /mnt/storage
	mount /dev/mapper/storage /mnt/storage
	cd /mnt/storage

And set up the volumes as defined above:

	btrfs subvolume create live
	btrfs subvolume create live/komputiloj
	btrfs subvolume create live/nextcloud
	btrfs subvolume create live/gitea
	btrfs subvolume create snapshots
	btrfs subvolume create backups
	btrfs subvolume create archives

### Resizing

To make the storage size bigger:

1. Resize the volume through Hetzner's web interface.
2. Reboot the server for LUKS to pick up the new size.
3. Resend keys with `komputiloj send-keys-to-$HOST`.
4. Wait until `/mnt/storage` is mounted.
5. Resize the filesystem with `btrfs filesystem resize max /mnt/storage`.

(Maybe we can replace steps 2 to 4 with a single remount step.)

## Install NixOS on a Hetzner VPS

On Hetzner, create a virtual machine with the following settings:

- Debian 10
- an SSH key
- the volume `storage` attached
- the following cloud-init user data:
  
	  #!/bin/sh
	  curl https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect | NIX_CHANNEL=nixos-20.09 bash

It will go and install NixOS on itself.
If you go to the virtual console, you will see a Debian login prompt
and not much seems to happen, but just wait until the server reboots
and shows a NixOS login prompt.

While you wait,
give the server a name in DNS and set the reverse DNS in the Hetzner Cloud Console.
Bind the name to a variable on your local machine for the later instructions to work:

	HOST=gently2.radstand.nl

If you don't believe want to check if the install is still running,
you can connect (untrusted, so what you'll see could be manipulated by a MITM)
with the following command and run `top` for instance:

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$HOST

After the server has rebooted into NixOS,
tell the server (or the server of a MITM, but this is harmless)
to make its host key fingerprint visible on the virtual console
with this filthy command:

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$HOST bash <<EOF
	fingerprint=$(ssh-keygen -l -f /etc/ssh/ssh_host_ed25519_key)
	sed "s/^}$/services.mingetty.helpLine = \"$fingerprint\";\n}/" -i /etc/nixos/configuration.nix
	nixos-rebuild switch
	EOF

Now spam the login prompt on the online virtual console until the welcome text is redisplayed,
and it will show the host fingerprint.
Establish trust by connecting through SSH and comparing the fingerprint:

	ssh root@$HOST

### Root password

Just one more thing before we go on to deploying our host config.
We want to have a root password so we can get into the system through the online virtual console.

	pwgen -s 10 | wachtwoord new passwords/root@$HOST
	printf "root:%s" "$(wachtwoord cat passwords/root@$HOST)" | ssh root@$HOST chpasswd

To make this password permanent, see the next non-provider-specific section.

### Cleanup

The system installed by Hetzner is no longer needed:

	ssh root@$HOST rm -rf /old-root

### A note on Hetzner's rescue system

To enter Hetzner's rescue system
(I haven't needed it during install yet),
go to the server details on the Hetzner Cloud Console,
go to RESCUE,
and click ENABLE RESCUE & POWER CYCLE.

The rescue OS has its keyboard set to German by default,
and has other annoyances such not pressing shift on paste
(changing `_` to `-` and `*` to `8`, etcetera),
which make it very hard to use through the virtual console.

We can fix the keyboard without needing to trust the server:

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$HOST bash <<EOF
	sed -i 's/^XKBLAYOUT=.*$/XKBLAYOUT="us"/' /etc/default/keyboard
	setupcon --force --save
	EOF

Now we can log into the server through the virtual console,
pasting the password displayed on the RESCUE page.

TODO describe how to establish trust with the rescue system
so we can use SSH and ignore the annoying virtual console.

## Install NixOS on a Tilaa VPS

On Tilaa, create a virtual machine with Debian 10.
We're going to mogrify it into a NixOS machine.

Fill in hostname:

	HOST=servilo-1.tilaa.cloud

Establish trusted SSH:

- Log in on the virtual console through Tilaa and run
  `ssh -o visualhostkey=yes localhost`  
  (don't accept, we just wanted to see the key)
- Verify server key and add own key using
  `ssh-copy-id -o visualhostkey=yes root@$HOST`
  (confirm only if the hostkey ascii art matches)

Now install NixOS with one easy command:

	curl https://raw.githubusercontent.com/jleeuwes/nixos-infect/master/nixos-infect |
	ssh root@$HOST PROVIDER=tilaa bash -x

It will reboot when done.

Your authorized SSH key is copied to the new install,
but the host key is not.
Moreover, the `root` account no longer has a password.
So verify the host like this:

1. `ssh root@$HOST`, which will fail.
2. Remove offending line from `known_hosts`.
3. `ssh -o visualhostkey=yes root@$HOST` again, accepting the host key.
4. Run `passwd` on the VPS to reset the `root` password to the one Tilaa generated.
5. Log in on the virtual console through Tilaa and run
   `ssh -o visualhostkey=yes localhost`  
  (don't accept, we just wanted to see the key).
6. If the hostkey ascii art matches, trust is established.
   If not, you have been MITMed and you should destroy the VPS.

### Stuff maybe useful for further automation

Snippet to find the id of the right virtual machine through the Tilaa API:

	curl -s -u $TILAA_API_USER:$TILAA_API_PASS https://api.tilaa.com/v1/virtual_machines | jq '.virtual_machines[] | select(.name=="'$HOST'") | .id'

## Make root password permanent

We use immutable users in our deployment configuration,
and we don't want to put our password in that configuration,
so we configure the root password to be read from `/root/password`.
To make this work, the hashed password needs to be put there.
Assuming the password is already set on the server:

	ssh root@$HOST bash <<EOF
	sed -E 's/^root:([^:]+):.*$/\1/;t;d' /etc/shadow > /root/password
	chmod go-r /root/password
	EOF

## NixOS configuration deployment

See [the general readme](README.md).

## Other ways

We can probably automate this process some more if we need to spin up more VPSes,
but even like this it's quite hassle free.

An alternative (for Tilaa, which has more manual steps than the Hetzner way)
might be to use rescue mode to 'run' a NixOS ISO that way,
but this is not easy to automate.

Also, `nixos-infect` should one day be superseded by
<https://github.com/NixOS/nixpkgs/issues/2079>.

