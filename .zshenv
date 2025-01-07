source "$HOME/Projects/dotfiles/env_functions"

# Preferred editor for local and remote sessions
export EDITOR='vim'

path_prepend "${HOME}/.local/bin" PATH

# Java
export JAVA_8_HOME=$(/usr/libexec/java_home -v 8)
export JAVA_11_HOME=$(/usr/libexec/java_home -v 11)
export JAVA_17_HOME=$(/usr/libexec/java_home -v 17)
export JAVA_21_HOME=$(/usr/libexec/java_home -v 21)

# AWS
# AWSume alias to source the AWSume script
# https://awsu.me/utilities/awsume-configure.html#alias
alias awsume=". awsume"
# Auto-Complete function for AWSume
fpath=(~/.awsume/zsh-autocomplete/ $fpath)

# For Rye Python management tool:
fpath=(~/.zfunc "${fpath[@]}")

. "$HOME/.cargo/env"
