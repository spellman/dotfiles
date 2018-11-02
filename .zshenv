alias mux="tmuxinator"
alias update-apij="git submodule update --init && ./scripts/jakarta/pull && lein with-profile +s3 deps && lein data"
alias mfc="ln -sf manifest-chrome.json ~/Projects/ferret/resources/unpacked/manifest.json"
alias mff="ln -sf manifest-firefox.json ~/Projects/ferret/resources/unpacked/manifest.json"
alias scout-devel="USE_UNPACKED_EXTENSION=true lein devel"
alias scout-docker="DOCKER_USER_UID=1000 DOCKER_USER_HOME=/home/cort ./bin/enter"

# Preferred editor for local and remote sessions
export EDITOR='vim'

if [ -f ~/.profile ]; then
	  . ~/.profile
fi
