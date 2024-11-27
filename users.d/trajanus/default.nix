{
    name = "trajanus";
    fullName = "Marcus Ulpius Nerva Traianus Augustus";
    apps.thee = {
        enable = true;
        visibility = "private";
    };
    linux.uid = 70005;
    email = "trajanus@radstand.nl";
    isHuman = false;
    ssh.publicKeys.gently = ''
        ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIuuPNswtk0h92nZ4mNZdRFCgAd5K6tg81OEKVZjV0AZ git-annex@gently
    '';
}
