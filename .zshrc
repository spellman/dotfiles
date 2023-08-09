# echo "zshrc start\n"

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="avit"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  colored-man-pages
  docker
  git
  kube-ps1
  lein
  pip
  poetry
  python
  tmux
  tmuxinator
  nvm
)

source $ZSH/oh-my-zsh.sh

# User configuration

eval "$(starship init zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
  PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
fi

alias flush-dns="sudo killall -HUP mDNSResponder; sleep 2;"

alias mux="tmuxinator"

alias ipaddress="ifconfig | grep inet"

alias gsur="git submodule update --recursive"

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

alias gpg2="gpg"

listening() {
  if [ $# -eq 0 ]; then
    sudo lsof -iTCP -sTCP:LISTEN -n -P
  elif [ $# -eq 1 ]; then
    sudo lsof -iTCP -sTCP:LISTEN -n -P | grep -i --color $1
  else
    echo "Usage: listening [pattern]"
  fi
}

alias sz="source ~/.zshrc"

c() {
  selected=$(find ~/Projects/phylum/localdev ~/Projects/phylum/localdev/research ~/Projects/phylum ~/Projects ~/ ~/Documents ~/Projects/powerlifting-america -mindepth 1 -maxdepth 1 -type d | fzf --ansi)
  cd $selected
}

# Mass-rename file names
# Ex:
#     zmv '(*).jpeg' '$1.jpg'
# Ex:
#     zmv '(*)-backup.(*)' 'backups/$1.$2'
# Source: https://apple.stackexchange.com/a/361957
autoload zmv
alias zcp='zmv -C' zln='zmv -L'

# SSH
alias start_ssh_agent='eval "$(ssh-agent -s)"'

# Kubernetes
alias k="kubectl"

kpf() {
  # Maybe I don't want the --address 0.0.0.0 :/
  # e.g., `kubectl port-forward service/redis-master 6379:6379`
  kubectl port-forward $1 $2:$3 --address 0.0.0.0
}

alias kpg="kpf postgres-postgresql-0 5432 5432"
alias kr="kpf service/redis-master 6379 6379"

kapi() {
  kpf $1 8020 8080
}

kl() {
  selected=$(kubectl get pods | awk '{print $1}' | grep -v NAME | gtac | fzf --ansi)
  kubectl logs $selected
}

klf() {
  selected=$(kubectl get pods | awk '{print $1}' | grep -v NAME | gtac | fzf --ansi)
  kubectl logs -f $selected
}

alias port_forward_localdev_hbase="ssh -i ~/.ssh/stg.pem -L 9090:thrift.localdev.phylum.dev:9090 hadoop@thrift.localdev.phylum.dev"

kexec() {
  selected=$(kubectl get pods | awk '{print $1}' | grep -v NAME | gtac | fzf --ansi)
  kubectl exec $selected -i -t -- /bin/bash
}

# 2021-07-15: Older fpath stuff for zsh completions:
#   fpath=(/usr/local/share/zsh-completions $fpath)
# Homebrew update output now says to use the following.
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
fi
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# NPM
npmdl() {
  wget $(npm view $1 dist.tarball)
}

# Ruby
# 2021-07-15: Older means of auto-switching:
# source $(brew --prefix)/opt/chruby/share/chruby/chruby.sh
# source $(brew --prefix)/opt/chruby/share/chruby/auto.sh
# Now, https://github.com/postmodern/chruby#auto-switching recommends this:
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh

# Python
# export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
# export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"
# export PYENV_ROOT="$HOME/.pyenv"
# export PATH="$PYENV_ROOT/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# coreutils
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# Java
# "/usr/libexec/java_home -v1.8" points to a non-openjdk java installation.
# That may be what I want sometimes but I need the openjdk java for running a
# heuristic locally so I'm commenting that out and hard-coding the openjdk
# location for now.
# export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_8_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"
export JAVA_11_HOME=$(/usr/libexec/java_home -v 11)
export JAVA_17_HOME=$(/usr/libexec/java_home -v 17)

alias java8='export JAVA_HOME=$JAVA_8_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'
alias java17='export JAVA_HOME=$JAVA_17_HOME'

# default to Java 11
java11

# Rust
source "$HOME/.cargo/env"
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

# Scala
export PATH="$PATH:/Users/cort/Library/Application Support/Coursier/bin"

# Postgres
alias start_local_pg="pg_ctl -D /usr/local/var/postgres start"
alias stop_local_pg="pg_ctl -D /usr/local/var/postgres stop"
alias local_psql="psql postgresql://cort:@localhost:5432/postgres"

# Redis
alias start_local_redis="brew services start redis"
alias stop_local_redis="brew services stop redis"

# Phyum
alias phl='phylum -c ~/.config/phylum/local.settings.yaml'
alias phs='phylum -c ~/.config/phylum/staging.settings.yaml'

# Django
# `shell_plus` requires django-extensions
# `--notebook` uses a Jupyter Notebook, which requires jupyterlab
# alias shell_notebook="DJANGO_ALLOW_ASYNC_UNSAFE=true ./manage.py shell_plus --notebook"

export PATH="$HOME/.config/emacs/bin:$PATH"

if [ "$(command -v exa)" ]; then
  unalias -m 'll'
  unalias -m 'l'
  unalias -m 'la'
  unalias -m 'ls'
  alias ls='exa -G  --color auto --icons -a -s type'
  alias l='exa -l --color always --icons -a -s type'
fi

if [ -f /usr/local/bin/less ]; then
  alias less="/usr/local/bin/less"
fi

# lipq stuff, from `brew upgrade` output
# libpq is keg-only, which means it was not symlinked into /usr/local,
# because conflicts with postgres formula.
#
# If you need to have libpq first in your PATH, run:
#   echo 'export PATH="/usr/local/opt/libpq/bin:$PATH"' >> ~/.zshrc
#
# For compilers to find libpq you may need to set:
#   export LDFLAGS="-L/usr/local/opt/libpq/lib"
#   export CPPFLAGS="-I/usr/local/opt/libpq/include"
#
# For pkg-config to find libpq you may need to set:
#   export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig"

# echo "zshrc end\n"

source /Users/cort/.local/share/phylum/zshrc
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

# Golang
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

inspect_parquet() {
  pqrs cat $1 --json | jq . | less
}

alias nvim="CC=/usr/local/bin/gcc-12 nvim"

if [ -f ~/java-version.sh ] && type -p sbt &>/dev/null; then
  sbtw() {
    sh ~/java-version.sh && sbt "$@"
  }
fi

alias update_kitty="curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin"
