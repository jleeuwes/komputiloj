# TODO finish and test

ssh root@gently.radstand.nl mkdir /var/src/.populate
ssh root@gently.radstand.nl touch /var/src/.populate
ssh root@gently.radstand.nl nix-env -f '"<nixpkgs>"' -A --install git # without -A memory usage explodes!
# ... do the deploy ...
ssh root@gently.radstand.nl nix-env --uninstall '.*' # (we should now use system-wide git)
