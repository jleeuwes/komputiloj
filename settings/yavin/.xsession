function daemons() {
  gnome-settings-daemon &
  
  sleep 2

  late_settings
}

function late_settings() {
  xrdb -load $HOME/.Xresources
  numlockx on
}

daemons &

# gnome-keyring-daemon is al door PAM gestart (waardoor de login-keyring al
# ontgrendeld is). Nu geven we door dat we hem ook voor gpg willen gebruiken:
eval $(gnome-keyring-daemon --start --components=secrets,ssh,gpg)
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID


late_settings

desktop-decoratie

xmonad

# kill $GNOME_KEYRING_PID
