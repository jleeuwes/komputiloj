
# Make symlinks red, among other things
eval "$(dircolors --sh ~/.dircolors)"

export HISTCONTROL=ignoreboth

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
# (only relevant in v7 mode which is becoming the default)
alias ga="git -c annex.largefiles=nothing add"

