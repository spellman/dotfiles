if [ -f ~/.bashrc ]; then
	. ~/.bashrc
elif [ -f ${default_dir}Bashrc ]; then
	. ${default_dir}Bashrc;
fi

if [ -f ~/.profile ]; then
	. ~/.profile
fi

# Set default editor
export EDITOR=vim

# Android Studio
#export PATH="/usr/local/android-studio/bin:$PATH"
#export PATH="$HOME/Android/Sdk/tools:$PATH"
#export PATH="$HOME/Android/Sdk/platform-tools:$PATH"

# NVM
#export PATH="$HOME/node_modules/.bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

source "$HOME/.cargo/env"

# >>> coursier install directory >>>
export PATH="$PATH:/Users/cort/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<

#AWSume alias to source the AWSume script
alias awsume="source \$(pyenv which awsume)"

#Auto-Complete function for AWSume
_awsume() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts=$(awsume-autocomplete)
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    return 0
}
complete -F _awsume awsume
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
