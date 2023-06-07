## The goal

Have a unified persistent configuration and a way to rebuild it easily.
I'm making it public so I can't loose it,
and maybe someone might find something useful in there for their own config.

## How to use this (on scarif)

1. Clone this repository into `~/komputiloj`:
    ```
    cd
    git clone git@github.com:jleeuwes/komputiloj.git
    cd komputiloj
    git submodule init    # some external repo(s) used by serviloj
    git submodule update  # some external repo(s) used by serviloj at the moment
    ```
2. Make some symlinks:
    ```
    mkdir -p /etc/nixos
    sudo ln -s ~/komputiloj/scarif/etc/nixos/* /etc/nixos/
    ```
3. (Re)build the NixOS configuration with `~/komputiloj/komputiloj nixos-rebuild switch`
4. Add a bunch of symlinks from `~` to files/dirs in `~/komputiloj/scarif/home/jeroen`
   (use `ls -A ~/komputiloj/scarif/home/jeroen` to see all the dotfiles).

(Unexplored alternative: set `NIXOS_CONFIG` environment variable instead of symlinking
stuff. See <https://github.com/argumatronic/home-nixos> and
<https://www.reddit.com/r/NixOS/comments/6zn5tv/manage_nixos_config_from_git_without_having_to/>
for some good (and bad) ideas.)

## Upgrading

Upgrade within same NixOS version:

	cd ~/komputiloj
	./komputiloj update-sources
	./komputiloj nixos-rebuild switch # local machine
	./komputiloj deploy               # server(s)

To upgrade the NixOS version for the local machine,
change `DEFAULT_NIXOS` in `~/komputiloj/komputiloj`,
then run the above commands.

To upgrade the NixOS version for our servers,
change `network.nixpkgs` in `serviloj.nix` to the major version you want,
then run the above.

## Concepts

There's a lot of ad-hoc stuff, but there are some concepts forming:

### Sources

A _source_ is some Nix value obtained from someplace else.
We use it instead of channels to pin our whole system.
On the filesystem it consists of:

- a `default.nix` containing an attrset with a `value` attribute
  and an optional `nix_path` attribute.
  The latter is useful if we want to use the source in our `NIX_PATH`.
- an `update` script that checks for a newer version of the thing we're
  sourcing and generates a new `default.nix` file.
- depending on the type of `update` script,
  some more files that define the sourced thing.

Sources seem similar to the dependency declaration and lock file of Nix flakes.
We might one day switch to flakes.

#### The `nixpkgs` source

One source, which must be named `nixpkgs`, is special:
`nixos-rebuild` is hardcoded to load `<nixpkgs/nixos>`, which in turn loads our
`configuration.nix`,
so by the time `configuration.nix` is evaluated, the 'main' `nixpkgs` is already chosen.
We work around this with some scripting that bootstraps a minimal `NIX_PATH`
from the `nixpkgs` entry in this file.
See the `komputiloj` script.

#### See also

Some resources that might come in handy and/or inspired this stuff:

- https://github.com/NixOS/nixpkgs/issues/62832
- https://github.com/NixOS/nixpkgs/issues/35411#issuecomment-368172579
- https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
- https://nix.dev/reference/pinning-nixpkgs

TODO: we might need to do some trickery to make sure the actively used sources are not gc'ed:
https://discourse.nixos.org/t/pinned-nixpkgs-keeps-getting-garbage-collected/12912/6

### Capsules

A _capsule_ provides different types of Nix- or komputiloj-related _objects_
(Nix values), like NixOS modules or komputiloj users.

A capsule can exist in one of two forms:

1. an attribute set with objects grouped by category.
2. a function taking other capsules and returning such an attribute set.

Capsules seem similar to the input/output part of Nix flakes.
We might one day switch to flakes.
I try to keep my capsules in line with my understanding of flakes.
That way I can pick the benefits of the flakes concept that I want,
without fully committing yet, while also making it potentially easy to switch later.

### Globules

Instead of defining and composing capsules,
I can also imagine a system of NixOS-module-like objects
but on the level of a whole komputiloj network.
We could define a structure for the whole komputiloj network (NixOps network
plus apps stuff) as a Nix value,
plus declare options that can output parts of that structure,
just like NixOS modules.

The module system in Nixpkgs is generic enough to do this.
As an example, I think <https://blog.jmgilman.com/writing-a-flake-library/> is
also using the module system outside of NixOS, but I haven't looked at it
in-depth.

For now, I am not taking this route,
because I already dislike debugging all the fix point magic of the NixOS module
system. I think the capsules route is more easily understandable and requires
less magic. It is more conducive to a 'follow the code' way of understanding
in which you can easily find and pop open the definition of some
variable/function/expression.

## Private/non-config data

Private and non-config data is stored in [datumoj](file:///home/jeroen/datumoj).
See <file:///home/jeroen/datumoj/README.md>.

TODO describe setting up datumoj from scratch,
including access to special remotes.

### Disaster recovery

TODO describe the concrete steps and test them.

The git part of datumoj lives on scarif and on gitea (storage volume).
The full annex content should be in at least two physically separate locations:
on scarif and on some external remote. Important rules to be able to
retrieve the content in case of scarif failing:

- The external remote's credentials MUST be stored in the _git_ part of datumoj.
  If it's part of the annex content, we would have a chicken-and-egg problem:
  we would need to retrieve datumoj's content in order to retrieve datumoj's content.
- We MUST have a way to access the git part of datumoj without already having a
  git checkout of datumoj.
  So, we need to be able to access our [Hetzner volume](serviloj/README.md),
  which hosts gitea data.
  This can be achieved by storing our Hetzner
  credentials and the LUKS password with our paper private key
  as described in [secrets management](secrets-management.md).
  Of course this will need to be kept in sync.
  TODO how can we 'automate' this?

## Useful inspiration

- <https://github.com/argumatronic/home-nixos>
- <https://chris-martin.org/2015/installing-nixos>

