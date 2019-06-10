## The goal

Have a unified persistent configuration and a way to rebuild it easily.
I'm making it public so I can't loose it,
and maybe someone might find something useful in there for their own config.

## The plan

1. Switch over my scarif NixOS config to this repo.
2. Switch over my scarif home config from my `home-overal` SVN crap to this repo.
3. Switch over my scarif content to some git-annex.
4. Roll this out to my other machines.

## How to use this

1. Clone this repository into `~/komputiloj`.
2. Make some symlinks:
    ```
    mkdir -p /etc/nixos
    sudo ln -s ~/komputiloj/scarif/etc/nixos/* /etc/nixos/
    ```
3. (Re)build the NixOS configuration. TODO

Alternative: set `NIXOS_CONFIG` environment variable instead of symlinking
stuff. See <https://github.com/argumatronic/home-nixos> and
<https://www.reddit.com/r/NixOS/comments/6zn5tv/manage_nixos_config_from_git_without_having_to/>
for some good (and bad) ideas.

## Useful inspiration

- <https://github.com/argumatronic/home-nixos>
- <https://chris-martin.org/2015/installing-nixos>

