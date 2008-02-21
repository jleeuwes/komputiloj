#env DISPLAY=:0.1 wmii &
#env DISPLAY=:0.1 gnome-terminal &
#env DISPLAY=:0.0 wmii   #we wachten op deze om af te sluiten

if xrandr --screen 1 --prop; then
  export DISPLAY=:0.1
  xsetroot -display :0.0 -solid black
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

ssh-agent -t 60 ~/bin/xmonad > $PIPE
