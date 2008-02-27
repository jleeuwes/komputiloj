#env DISPLAY=:0.1 wmii &
#env DISPLAY=:0.1 gnome-terminal &
#env DISPLAY=:0.0 wmii   #we wachten op deze om af te sluiten

if xrandr --screen 1 --prop; then
  echo Meerdere schermen
  export DISPLAY=:0.1
  xsetroot -display :0.0 -solid black
else
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
