#env DISPLAY=:0.1 wmii &
#env DISPLAY=:0.1 gnome-terminal &
#env DISPLAY=:0.0 wmii   #we wachten op deze om af te sluiten

if xrandr --screen 1 --prop; then
  #twee schermen zonder Xinerama
  SCHERMEN=2

  echo Meerdere schermen
  export DISPLAY=:0.1
  xsetroot -display :0.0 -solid black
  #xset -display :0.0 dpms force off
  #gnome-screensaver-command --inhibit --reason 'Screensaver crasht met meerdere schermen!' &
else
  SCHERMEN=1

  echo Enkel scherm
fi

unclutter -idle 1 -root &
gnome-settings-daemon &
~/bin/desktop-decoratie &

#zet muiscursor op achtergrond goed (verdraaid simpel achteraf):
xsetroot -cursor_name arrow

PIPE=$HOME/.xmonad-log

rm -f $PIPE
mkfifo $PIPE

~/bin/xmobar < $PIPE &

ssh-agent ~/bin/xmonad > $PIPE

if [ "$SCHERMEN" = "2" ]; then
  #xset -display :0.0 dpms force on
fi
