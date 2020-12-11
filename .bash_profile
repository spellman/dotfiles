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
