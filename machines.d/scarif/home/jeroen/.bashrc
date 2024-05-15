
# Make symlinks red, among other things
eval "$(dircolors --sh ~/.dircolors)"

PROMPT_COLOR="37;40m"
PS_WINDOW_TITLE_PART="\u@\h:\w"
PS_PROMPT_BLOCK_PART="\u@\h:\w"
PS_PROMPT_LAST_PART=" 𝄽"
PS1="\n\[\033[$PROMPT_COLOR\]\[\e]0;$PS_WINDOW_TITLE_PART\a\]$PS_PROMPT_BLOCK_PART\[\033[0m\]$PS_PROMPT_LAST_PART"

export HISTCONTROL=ignoreboth
export HISTSIZE=10000
export HISTFILESIZE=10000
# Store the time of commands, but also serves as a separator in the history
# file so that multiline commands are read back in as one command
# (as long as the whole bash_history file contains these timestamps!):
export HISTTIMEFORMAT='%F %T: '
# Write commands to history instantly for use in new shells:
export PROMPT_COMMAND="$PROMPT_COMMAND ; history -a"
# Prevent overwriting of history file if full:
shopt -s histappend

sz() {
	local size
	size="$(printf "%s" "$1"|egrep ^[0-9]+$)"
	if test -z "$size"; then
		echo "Invalid terminal font size $size" >&2
		return 1
	else
		echo "Setting terminal font size to $size" >&2
		sed -Ei 's/^(FontName=.*) [0-9]+$/\1 '"$size"'/' ~/.config/xfce4/terminal/terminalrc
	fi
}

# Alias for git add that bypasses git-annex
# (preferably configure largefiles correctly)
alias ga="git annex add --force-small"

alias lpr-bw="lpr -o CololModel=Gray -o print-color-mode=monochrome"

if [[ -e ~/.plan ]]; then
	cat ~/.plan
fi
