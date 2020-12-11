alias flush-dns="sudo killall -HUP mDNSResponder; sleep 2;"

alias mux="tmuxinator"

alias ipaddress="ifconfig | grep inet"

# APIJ
alias update-apij="git submodule update --init && ./scripts/pull-all-client-resources && lein with-profile +s3 deps && lein data"
alias apij-clj-repl="clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version \"0.6.0\"}}}' -A:dev -m nrepl.cmdline"
alias scout-apij="CORS_ORIGIN='chrome-extension://dmpnijbhahiachaigljekjimblljmlpi' ./dev/server"

# Scout
alias scout-devel="USE_UNPACKED_EXTENSION=true lein devel"
alias scout-scraper="lein with-profile test-integration cljsbuild once scraper"
alias scout-docker="DOCKER_USER_UID=1000 DOCKER_USER_HOME=/home/cort ./bin/enter"
alias mfc="ln -sf manifest-chrome.json ~/Projects/ferret/resources/unpacked/manifest.json"
alias mff="ln -sf manifest-firefox.json ~/Projects/ferret/resources/unpacked/manifest.json"
# Root directory for Scout integration test data
export LOCAL_TEST_DATA_ROOT=~/Projects/ferret/test/resources/dev-scout-regression-test-data

# GPG
alias gpg2="gpg"
GPG_TTY=$(tty)
export GPG_TTY

# Preferred editor for local and remote sessions
export EDITOR='vim'

if [ -f ~/.profile ]; then
	  . ~/.profile
fi
source "$HOME/.cargo/env"
