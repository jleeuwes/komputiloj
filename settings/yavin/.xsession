gnome-settings-daemon
gnome-power-manager &

eval $(gnome-keyring-daemon)
export GNOME-KEYRING-SOCKET
export GNOME-KEYRING-PID

numlockx on

xrdb -load $HOME/.Xresources

xmonad

kill $GNOME-KEYRING-PID
