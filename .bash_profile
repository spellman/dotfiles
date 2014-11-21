# Note that ~/.profile includes ~/.bashrc.
source ~/.profile

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"

eval "$(rbenv init -)"
[[ -s /home/cort/.nvm/nvm.sh ]] && . /home/cort/.nvm/nvm.sh # This loads NVM
[[ -r $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion # Bash completion
