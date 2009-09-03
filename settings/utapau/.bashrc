# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#verplaatst van .bash_profile naar hier:
# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi

#geen dubbelen in bash_history
export HISTCONTROL=ignoredups

#zet de huidige directory ook in het pad. Wel als laatst!
#PATH="${PATH}":.

#extra handigheden toevoegen aan PATH:
PATH="${PATH}":/opt/ozone-1.2.1-beta/bin:/opt/maven-2.0.7/bin:~/.cabal/bin

#bedoeld om eclipse java 6 te laten gebruiken
export JAVA_HOME="/usr/lib/jvm/java-6-sun"

#export SVN_EDITOR=mcedit  #de SVN-editor (duh) voor berichtjes bij commits (nog niet getest of dit werkt)

# don't put duplicate lines in the history. See bash(1) for more options
#export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
alias ls='ls -A --color=auto -p'
alias ll='ls -lh'
#alias la='ls -A'
#alias l='ls -CF'

#handig: directory/file size
alias duh='du -h'
alias dus='du -hs'

alias up='. goup'

#nautilus zonder desktop
alias nautilus='nautilus --no-desktop'

#pgrep met naam
alias pgrep='pgrep -l'

alias cssh='ssh -1 -l jleeuwes -i ~/uni/key.ppk shell.students.cs.uu.nl'
alias cscp='scp -1 -i ~/uni/key.ppk'
#alias cscp='scp -1 -i ~/uni/key.pkk

# laat vim snel opstarten in screens die hun eventuele X-sessie zijn ontsnapt
# zie ook .vimrc http://markmail.org/message/nwkwulaj4wiuuouu
alias vim='vim -X'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" -a -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
#case "$TERM" in
#xterm-color|rxvt*)
    PS1='${debian_chroot:+($debian_chroot)}\u\[\e[1;35m\]@\h\[\e[m\]:\[\e[1;34m\]\w\[\e[m\]\$ '
#    ;;
#*)
#    # PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#    ;;
#esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
    ;;
*)
    ;;
esac

# Define your own aliases here ...
#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
#if [ -f /etc/bash_completion ]; then
#    . /etc/bash_completion
#fi

# niet schermwissen
setterm -blank > /dev/null
# niet piepen
#setterm -blength 0
# wel piepen, maar niet superirritant
setterm -blength 30 -bfreq 500

vergeet-mij-niet
