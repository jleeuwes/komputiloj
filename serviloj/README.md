# Serviloj

Work in progress to set up a more
[cattle-like](https://medium.com/@Joachim8675309/devops-concepts-pets-vs-cattle-2380b5aab313)
(set of) server(s) for running my online services.

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

	VPS=gently2.radstand.nl

If you don't believe want to check if the install is still running,
you can connect (untrusted, so what you'll see could be manipulated by a MITM)
with the following command and run `top` for instance:

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$VPS

After the server has rebooted into NixOS,
tell the server (or the server of a MITM, but this is harmless)
to make its host key fingerprint visible on the virtual console
with this filthy command:

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$VPS bash <<EOF
	fingerprint=$(ssh-keygen -l -f /etc/ssh/ssh_host_ed25519_key)
	sed "s/^}$/services.mingetty.helpLine = \"$fingerprint\";\n}/" -i /etc/nixos/configuration.nix
	nixos-rebuild switch
	EOF

Now spam the login prompt on the online virtual console until the welcome text is redisplayed,
and it will show the host fingerprint.
Establish trust by connecting through SSH and comparing the fingerprint:

	ssh root@$VPS

### Root password

Just one more thing before we go on to NixOps deployment.
We want to have a root password so we can get into the system through the online virtual console.

	pwgen -s 10 | wachtwoord new passwords/root@$VPS
	printf "root:%s" "$(wachtwoord cat passwords/root@$VPS)" | ssh root@$VPS chpasswd

To make this password permanent, see the next non-provider-specific section.

### Cleanup

The system installed by Hetzner is no longer needed:

	ssh root@$VPS rm -rf /old-root

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

	ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$VPS bash <<EOF
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

	VPS=servilo-1.tilaa.cloud

Establish trusted SSH:

- Log in on the virtual console through Tilaa and run
  `ssh -o visualhostkey=yes localhost`  
  (don't accept, we just wanted to see the key)
- Verify server key and add own key using
  `ssh-copy-id -o visualhostkey=yes root@$VPS`
  (confirm only if the hostkey ascii art matches)

Now install NixOS with one easy command:

	curl https://raw.githubusercontent.com/jleeuwes/nixos-infect/master/nixos-infect |
	ssh root@$VPS PROVIDER=tilaa bash -x

It will reboot when done.

Your authorized SSH key is copied to the new install,
but the host key is not.
Moreover, the `root` account no longer has a password.
So verify the host like this:

1. `ssh root@$VPS`, which will fail.
2. Remove offending line from `known_hosts`.
3. `ssh -o visualhostkey=yes root@$VPS` again, accepting the host key.
4. Run `passwd` on the VPS to reset the `root` password to the one Tilaa generated.
5. Log in on the virtual console through Tilaa and run
   `ssh -o visualhostkey=yes localhost`  
  (don't accept, we just wanted to see the key).
6. If the hostkey ascii art matches, trust is established.
   If not, you have been MITMed and you should destroy the VPS.

### Stuff maybe useful for further automation

Snippet to find the id of the right virtual machine through the Tilaa API:

	curl -s -u $TILAA_API_USER:$TILAA_API_PASS https://api.tilaa.com/v1/virtual_machines | jq '.virtual_machines[] | select(.name=="'$VPS'") | .id'

## Make root password permanent

We use immutable users in our deployment configuration,
and we don't want to put our password in that configuration,
so we configure the root password to be read from `/root/password`.
To make this work, the hashed password needs to be put there.
Assuming the password is already set on the server:

	ssh root@$VPS bash <<EOF
	sed -E 's/^root:([^:]+):.*$/\1/;t;d' /etc/shadow > /root/password
	chmod go-r /root/password
	EOF

## NixOps

TODO

Now update your NixOps deployment and deploy!

## Other ways

We can probably automate this process some more if we need to spin up more VPSes,
but even like this it's quite hassle free.

An alternative (for Tilaa, which has more manual steps than the Hetzner way)
might be to use rescue mode to 'run' a NixOS ISO that way,
but this is not easy to automate.

Also, `nixos-infect` should one day be superseded by
<https://github.com/NixOS/nixpkgs/issues/2079>.

