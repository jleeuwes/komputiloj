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
3. (Re)build the NixOS configuration with `~/komputiloj/nixos-rebuild switch`
4. Add a bunch of symlinks from `~` to files/dirs in `~/komputiloj/scarif/home/jeroen`
   (use `ls -A ~/komputiloj/scarif/home/jeroen` to see all the dotfiles).

(Unexplored alternative: set `NIXOS_CONFIG` environment variable instead of symlinking
stuff. See <https://github.com/argumatronic/home-nixos> and
<https://www.reddit.com/r/NixOS/comments/6zn5tv/manage_nixos_config_from_git_without_having_to/>
for some good (and bad) ideas.)

## Upgrading

Upgrade within same NixOS version:

	cd ~/komputiloj
	./update-sources
	./nixos-rebuild switch # local machine
	cd serviloj
	./deploy               # server(s)

To upgrade the NixOS version for the local machine,
change the version in `~/komputiloj/sources.d/nixpkgs/channel_url`,
then run the above commands.

To upgrade the NixOS version for our servers,
change `network.nixpkgs` in `serviloj.nix` to the major version you want,
then run the above.
TODO switch serviloj over to the nixpkgs 'channel' as well,
so local and remote machines are on the same major version.

## Private/non-config data

Private and non-config data is stored in [datumoj](file:///home/jeroen/datumoj).
See <file:///home/jeroen/datumoj/README.md>.

TODO describe setting up datumoj from scratch,
including access to special remotes.

Also important: [secrets management](secrets-management.md).

## Useful inspiration

- <https://github.com/argumatronic/home-nixos>
- <https://chris-martin.org/2015/installing-nixos>

