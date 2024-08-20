eval "$(starship init zsh)"

setopt APPEND_HISTORY
HISTSIZE=50000
SAVEHIST=10000

# fzf fuzzy finder
source <(fzf --zsh)

# 3S `ls` command
if [ "$(command -v eza)" ]; then
  unalias -m 'll'
  unalias -m 'l'
  unalias -m 'la'
  # Don't remove this. Scripts use it with options supported by ls but not eza.
  # unalias -m 'ls'
  # alias ls='eza -G  --color auto --icons -a -s type'
  alias l='eza -l --color always --icons -a -s type'
fi

# Navigate between projects
p() {
  # TODO: Use fd instead of find?
  selected=$(find ~/Projects ~/Projects/dotfiles ~/Projects/powerlifting-america ~ ~/Documents -mindepth 1 -maxdepth 1 -type d | fzf --ansi)
  cd $selected
}

# GPG
alias gpg2="gpg"

# SSH
alias start_ssh_agent='eval "$(ssh-agent -s)"'

# Various
alias flush-dns="sudo killall -HUP mDNSResponder; sleep 2;"
alias ipaddress="ifconfig | grep inet"
alias npmdl="download-npm-package-archive"
alias pd="inspect-parquet"

# AWS
alias ad="awsume default"

# Kubernetes
# See ~/.local/bin for scripts referenced. E.g.,`kpf`
# alias k="kubectl"
# alias kexec="exec-into-selected-kubernetes-pod"
# alias kl="view-log-of-selected-kubernetes-pod"
# alias klf="follow-log-of-selected-kubernetes-pod"
# alias kpf="kubernetes-port-forward"
# alias kpg="kubernetes-port-forward postgres-postgresql-0 5432 5432"
# alias kr="kubernetes-port-forward service/redis-master 6379 6379"

# Postgres
alias start_local_pg="pg_ctl -D /usr/local/var/postgres start"
alias stop_local_pg="pg_ctl -D /usr/local/var/postgres stop"
alias local_psql="psql postgresql://cort:@localhost:5432/postgres"

# Redis
alias start_local_redis="brew services start redis"
alias stop_local_redis="brew services stop redis"

# Java
# `JAVA_*_HOME` environment variables are set and exported in ~/.zshenv
alias java8='export JAVA_HOME=$JAVA_8_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'
alias java17='export JAVA_HOME=$JAVA_17_HOME'
alias java21='export JAVA_HOME=$JAVA_21_HOME'

# default to Java 21
java21

# Enable chruby
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby ruby-3.3.1

# zsh auto-suggestions
source ~/.local/share/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# zsh syntax highlighting
source ~/.local/share/zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

# iTerm2 shell integration:
# https://iterm2.com/documentation-shell-integration.html
source ~/.iterm2_shell_integration.zsh

alias emacs="emacsclient --create-frame --alternate-editor=emacs"
