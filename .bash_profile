if [ -f ~/.bashrc ]; then
	. ~/.bashrc
elif [ -f ${default_dir}Bashrc ]; then
	. ${default_dir}Bashrc;
fi

# Android Studio
export PATH="/opt/android-studio/bin:$PATH"
export PATH="$HOME/Android/Sdk/tools:$PATH"
export PATH="$HOME/Android/Sdk/platform-tools:$PATH"

# NVM
export PATH="$HOME/node_modules/.bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"

eval "$(rbenv init -)"
[[ -s /home/cort/.nvm/nvm.sh ]] && . /home/cort/.nvm/nvm.sh # This loads NVM
[[ -r $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion # Bash completion
