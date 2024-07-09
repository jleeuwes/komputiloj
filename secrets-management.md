# Secrets management

The intended audience of this document is me;
it is a public document so I won't loose it,
but it's not necessarily useful to anyone else.

## Passwords for personal use

Passwords, tokens, etcetera are encrypted using `wachtwoord`,
which encrypts secrets with my age key.

The encrypted secrets are stored in the datumoj repository,
directly in git (not annexed).

These secrets are backed up by virtue of datumoj being duplicated/synchronizated
between multiple devices,
as long as:

1. I have access to my age (and gpg) key.
2. I have access to a datumoj git repo.

See disaster recovery in [README.md](README.md).

### age

On scarif I have an age key (`~/passwords/.age/identity.age`),
encrypted with a passphrase.

All new passwords made with `wachtwoord new` are encrypted with this age key.

#### Backups

Proper backup of my age key is imperative.

My private key:

- Is stored on my main workstation encrypted with a passphrase.
- Is stored on paper unencrypted.
- TODO: should be stored on another piece of paper in a separate location.

Alternatively to the TODO, if I were to use multiple workstations and encrypt
my passwords to all the workstations' keys,
this would provided enough redundancy.

Note that, contrary to GPG keys, age keys are very small,
so there is no need to have a digital backup to prevent lots of typing
from paper.

The public key can be derived from the private key,
so no special care is needed there.

### GPG

Most passwords are still encrypted with GPG
but I'm phasing this out.

No new passwords should be encrypted using this method.

The GPG key is considered compromised since I lost
my USB-stick with a backup on it.
This is not an immediate problem,
because the encrypted password files are not public;
an attacker would also need to gain access to datumoj.

My jeroen@lwstn.eu GPG key *WAS* at the center of my secrets management.
It has a primary key for GPG identity management and subkeys for encryption and signing.
The encryption and signing secret keys will be available on my workstation.
The primary secret key will be placed on a removable medium for use when needed.
Not sure what the status of this was.

See [this Debian guide](https://wiki.debian.org/Subkeys) for information about
removing the primary key from a keyring and using it from another location on
the removable medium.

#### Backups

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

## System secrets

We are trying a new scheme here.
Credentials for use on gently are encrypted with a host-specific
master age key.
These encrypted credentials are checked in to git
and are transfered to gently through the nix store.

When deploying, the master key is decrypted and transfered through SSH,
then the credentials are decypted on the host using the master key.

Each host has:

1. `masterkey.age`: an age secret key encrypted with my personal age key.
   Generated with `age-keygen | wachtwoord new masterkey.age`.
2. `masterkey.pub`: the public key corresponding to `masterkey.age`.

Secrets can be encrypted with a masterkey like this:

```
age -R masterkey.pub -a > secret.age
```

TODO This should be automated using `encryptedPath` and `encryptedFor`
in the secret's definition in nix.
