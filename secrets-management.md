# Secrets management

The intended audience of this document is me;
it is a public document so I won't loose it,
but it's not necessarily useful to anyone else.

## GPG

My jeroen@lwstn.eu GPG key will be at the center of my secrets management.
It has a primary key for GPG identity management and subkeys for encryption and signing.
The encryption and signing secret keys will be available on my workstation.
The primary secret key will be placed on a removable medium for use when needed.

See [this Debian guide](https://wiki.debian.org/Subkeys) for information about
removing the primary key from a keyring and using it from another location on
the removable medium.

### Backups

Proper backups of my GPG keys is imperative.

My public keys:

- Must be stored on at least two easily accessible devices [TODO]
- Must be stored in a publicly accessible place on the web
	- My new key is [here in this repo](jeroen@lwstn.eu.pub.asc)
	- My old keys (which should be phased out) are already floating around [on
	  keyservers](https://keyserver.ubuntu.com/pks/lookup?search=jeroenleeuwestein&fingerprint=on&op=index)

My private keys:

- Must be stored on my main workstation encrypted with a passphrase.
- Must be stored on paper in two physically separate locations. [TODO]
	- We use [paperkey](https://www.jabberwocky.com/software/paperkey/) for
	  this, which does not include the public part, so the public key backups
	  above are very important.

## SSH

Public keys, `known_hosts` files and `authorized_keys` files can be checked in
in komputiloj to manage access.

Some private keys may be regarded as secrets as described below.
TODO not sure, work this out.

## Passwords and other secrets

Passwords, tokens, etcetera are encrypted using `wachtwoord`,
which encrypts secrets with my GPG key and optionally with the GPG key of some
system user that needs access to it. Such system GPG keys can in turn also be
encrypted using the `wachtwoord` program in this repository.

The encrypted secrets are stored in the datumoj repository if they are for
personal use, or in the komputiloj repository if they must be available for
unattended use by some device configured in komputiloj.

Komputiloj being public and datumoj being duplicated/synchronizated between
multiple devices will make sure these secrets are backed up.
(But note that we need to ensure we can always access a copy! See [README.md].)

