# Always have an up-to-date NIX_PATH for nix-shell
# TODO: replace with something less hacky, like a komputiloj shell command
# that works like nix-shell but with our pinned packages.
eval $(grep -E '^export NIX_PATH="' /etc/set-environment)

export NIX_SHELL_PRESERVE_PROMPT=1

__ps_colored() {
	printf '\001\033[%sm\002%s\001\033[0m\002' "$1" "$2"
}

# TODO window title-stuff fixen? Moet in konsole ook eerst worden ingesteld.
HOST_COLOR="31"
PS_PROMPT_BLOCK_PART="$(__ps_colored 1 '\u') in $(__ps_colored 1 '\w') on $(__ps_colored "1;$HOST_COLOR" '\h')"
PS_WINDOW_TITLE_PART="\u in \h on \w"
PS_PROMPT_LAST_PART="\n⟫ "
# PS1="\n\$(__ps_exitcode)\n\[\033[0\]\[\e]0;$PS_WINDOW_TITLE_PART\a\]$PS_PROMPT_BLOCK_PART\$(__ps_nixshell)\$(__ps_screen)$PS_PROMPT_LAST_PART\[\033[0m\]"
PS1="\n\$(__ps_exitcode)\n$PS_PROMPT_BLOCK_PART\$(__ps_nixshell)\$(__ps_screen)$PS_PROMPT_LAST_PART\[\033[0m\]"

__ps_exitcode() {
	local exitcode=$?
	if [[ $exitcode -eq 0 ]]; then
		__ps_colored "1;34" ":)"
	else
		__ps_colored "1;31" ":( $exitcode"
	fi
}

__ps_nixshell() {
	local pkgs
	if [[ -v IN_NIX_SHELL ]]; then
		# # NOTE: for nested nix-shells, only shows packages of the last one
		pkgs=( $(printf '%s' "$buildInputs" | sed -E 's/\/nix\/store\/[0-9a-z]+-//g') )
		case ${#pkgs[@]} in
		0)
			printf '\ninside %s nix-shell' \
				"$IN_NIX_SHELL"
		;;
		1)
			printf '\ninside %s nix-shell with package %s' \
				"$IN_NIX_SHELL" "$(__ps_colored 1 "${pkgs[0]}")"
		;;
		*)
			printf '\ninside %s nix-shell with packages %s' \
				"$IN_NIX_SHELL" "$(__ps_colored 1 "${pkgs[*]}")"
		;;
		esac
	fi
}

__ps_screen() {
	if [[ -v STY ]]; then
		printf '\ninside screen session %s' \
			"$(__ps_colored 1 "$STY")"
	fi
}

__ps_shelllevel() {
	# NOTE: this is not very useful when using screen
	if [[ $SHLVL -gt 1 ]]; then
		printf 'inside %d levels of shell\n' $SHLVL
	fi
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

__prompt_command() {
	__prompt_command_history
}
if [[ ! -v BASHRC_DONE_FOR_SHLVL || $BASHRC_DONE_FOR_SHLVL -lt $SHLVL ]];
then
	if [[ -v PROMPT_COMMAND ]]; then
		echo -n "PROMPT_COMMAND already set! Was: " >&2
		declare -p PROMPT_COMMAND >&2
	fi
	PROMPT_COMMAND=__prompt_command
	export BASHRC_DONE_FOR_SHLVL=$SHLVL
fi

export EDITOR=vim

export RCLONE_PASSWORD_COMMAND="wachtwoord cat /home/jeroen/passwords/RCLONE_CONFIG_PASS"

alias webcamshot='fswebcam --no-banner --png 9 webcamshot-"$(date -Iseconds)"'
alias webcam='vlc v4l2:///dev/video0'

qr-open-webcam() {
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
