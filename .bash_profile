if [ -f ~/.bashrc ]; then
	. ~/.bashrc
elif [ -f ${default_dir}Bashrc ]; then
	. ${default_dir}Bashrc;
fi

# Linuxbrew
export PATH="$HOME/.linuxbrew/bin:$PATH"
export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"

# Android Studio
export PATH="/usr/local/android-studio/bin:$PATH"
export PATH="$HOME/Android/Sdk/tools:$PATH"
export PATH="$HOME/Android/Sdk/platform-tools:$PATH"

# NVM
#export PATH="$HOME/node_modules/.bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
