# Always have an up-to-date NIX_PATH for nix-shell
# TODO: replace with something less hacky, like a komputiloj shell command
# that works like nix-shell but with our pinned packages.
eval $(grep -E '^export NIX_PATH="' /etc/set-environment)

__ps_colored() {
	printf '\[\\033[%sm\]%s\[\033[%sm\]' "$1" "$2" "$PROMPT_COLOR"
}

USER_COLOR="1;37"
HOST_COLOR="1;31"
PATH_COLOR="1;37"
PROMPT_COLOR="0"
PS_USER_PART="\[\033[${USER_COLOR}m\]\u\[\033[${PROMPT_COLOR}m\]"
PS_HOST_PART="\[\033[${HOST_COLOR}m\]\h\[\033[${PROMPT_COLOR}m\]"
PS_PATH_PART="\[\033[${PATH_COLOR}m\]\w\[\033[${PROMPT_COLOR}m\]"
PS_PROMPT_BLOCK_PART="${PS_USER_PART} in ${PS_PATH_PART} on ${PS_HOST_PART}"
PS_WINDOW_TITLE_PART="\u in \h on \w"
PS_PROMPT_LAST_PART="\n⟫ "
PS_EXIT=$(__ps_colored "1;34" ":)")
PS_FIXED_PART="\[\033[$PROMPT_COLOR\]\[\e]0;$PS_WINDOW_TITLE_PART\a\]$PS_PROMPT_BLOCK_PART$PS_PROMPT_LAST_PART\[\033[0m\]"
PS1="\n$PS_FIXED_PART"

__prompt_command_exitcode() {
	local exitcode=$?
	if [[ $exitcode -eq 0 ]]; then
		PS_EXIT=$(__ps_colored "1;34" ":)")
	else
		PS_EXIT=$(__ps_colored "1;31" ":( $exitcode")
	fi
	PS1="\n$PS_EXIT\n$PS_FIXED_PART"
}

export HISTCONTROL=ignoreboth
export HISTSIZE=10000
export HISTFILESIZE=10000
# Store the time of commands, but also serves as a separator in the history
# file so that multiline commands are read back in as one command
# (as long as the whole bash_history file contains these timestamps!):
export HISTTIMEFORMAT='%F %T: '
# Write commands to history instantly for use in new shells:
__prompt_command_history() {
	history -a
}
# Prevent overwriting of history file if full:
shopt -s histappend

if [[ -v PROMPT_COMMAND ]]; then
	echo -n "PROMPT_COMMAND already set! Was: " >&2
	declare -p PROMPT_COMMAND >&2
fi
__prompt_command() {
	__prompt_command_exitcode # must go first
	__prompt_command_history
}
export PROMPT_COMMAND=__prompt_command

export EDITOR=vim

alias webcamshot='fswebcam --no-banner --png 9 webcamshot-"$(date -Iseconds)"'
alias webcam='vlc v4l2:///dev/video0'

qr-open() {
	local url
	url=$(zbarcam --oneshot --raw)
	if [[ $url == http://* || $url == https://* ]]; then
		xdg-open $url
	else
		printf "Don't know what to do with %q\n" "$url"
	fi
}

if [[ -e ~/.plan ]]; then
	cat ~/.plan
fi
