# Servilo

Work in progress to set up a more
[cattle-like](https://medium.com/@Joachim8675309/devops-concepts-pets-vs-cattle-2380b5aab313)
(set of) server(s) for running my online services.

The idea is to put services in separate containers
and deploy them using NixOps;
first to one server but this should be easily expandable.

## NixOS on tilaa.com

On Tilaa, create a virtual machine with Debian 10.
We're going to mogrify it into a NixOS machine.

Fill in hostname and Tilaa-generated password:

	VPS=servilo-1.tilaa.cloud
	 VPS_PASSWORD=...

Establish trusted SSH:

- Log in on the virtual console of the VPS and run
  `ssh-keygen -l -v -f /etc/ssh/ssh_host_ecdsa_key.pub`
- Verify server key and add own key using
  `ssh-copy-id -o visualhostkey=yes root@$VPS`
  (confirm only if the hostkey ascii art matches)

Step 1 from <https://github.com/jleeuwes/nixos-in-place>:

	ssh root@$VPS apt-get update
	ssh root@$VPS apt-get install -y squashfs-tools git wget

Adding swap is unnecessary because Tilaa preconfigures some.

Download the tool:

	ssh root@$VPS git clone https://github.com/jleeuwes/nixos-in-place.git

Customize root password to not have an open server:

	ssh root@$VPS cp nixos-in-place/default-extra-config.nix /root
	ssh root@$VPS sed -i 's/"nixos"/"'$VPS_PASSWORD'"/' /root/default-extra-config.nix
	ssh root@$VPS cat /root/default-extra-config.nix # just to check

Then install NixOS and reboot:

	ssh root@$VPS nixos-in-place/install
	ssh root@$VPS reboot

Now redo the verification + key install steps above.
(TODO reuse hostkey? I couldn't get it to work. Maybe because of algorithm choices differing between Debian and NixOS 16)
(Note: it's actually OK to have to reinstall our key,
because that way we can check whether the default password has been
successfully overwritten.)

Now upgrade NixOS.
Prepare for a long wait because we need to make several stops along the way,
because Nix has to be updated for the newest Nixpkgs
but the newer Nixpkgses are too new for the installed version of Nix.

	ssh root@$VPS nix-channel --add https://nixos.org/channels/nixos-17.09 nixos
	ssh root@$VPS nixos-rebuild switch --upgrade
	ssh root@$VPS nix-channel --add https://nixos.org/channels/nixos-18.09 nixos
	ssh root@$VPS nixos-rebuild switch --upgrade
	ssh root@$VPS nix-channel --add https://nixos.org/channels/nixos-19.09 nixos
	ssh root@$VPS nixos-rebuild switch --upgrade
	ssh root@$VPS nix-collect-garbage

Now reboot and you have the newest NixOS!

## To be continued

Now apply some Nixops magic.
That's why we did this in the first place.

## Stuff maybe useful for further automization

Snippet to find the id of the right virtual machine through the Tilaa API:

	curl -s -u $TILAA_API_USER:$TILAA_API_PASS https://api.tilaa.com/v1/virtual_machines | jq '.virtual_machines[] | select(.name=="'$VPS'") | .id'

## Better ways

This process is anoying and not really workable
for provisioning more virtual machines.
Also, it leaves a bit of a weird setup in which
the actual system is installed inside `/old-root/nixos`.

We should try <https://github.com/elitak/nixos-infect>
or wait for <https://github.com/NixOS/nixpkgs/issues/2079>.

