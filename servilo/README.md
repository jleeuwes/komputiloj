# Servilo

Work in progress to set up a more
[cattle-like](https://medium.com/@Joachim8675309/devops-concepts-pets-vs-cattle-2380b5aab313)
(set of) server(s) for running my online services.

The idea is to put services in separate containers
and deploy them using NixOps;
first to one server but this should be easily expandable.

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

## To be continued

Now apply some Nixops magic.
That's why we did this in the first place.

## Stuff maybe useful for further automization

Snippet to find the id of the right virtual machine through the Tilaa API:

	curl -s -u $TILAA_API_USER:$TILAA_API_PASS https://api.tilaa.com/v1/virtual_machines | jq '.virtual_machines[] | select(.name=="'$VPS'") | .id'

## Other ways

We can probably automate this process some more if we need to spin up more VPSes,
but even like this it's quite hassle free.

An alternative might be to use rescue mode to 'run' a NixOS ISO that way,
but this is not easy to automate.

Also, `nixos-infect` should one day be superseded by
<https://github.com/NixOS/nixpkgs/issues/2079>.

