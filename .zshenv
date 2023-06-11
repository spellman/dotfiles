# echo "zshenv start\n"

GPG_TTY=$(tty)
export GPG_TTY

# Preferred editor for local and remote sessions
export EDITOR='vim'

# echo "zshenv end\n"

export PATH="/usr/local/bin:$PATH"

#AWSume alias to source the AWSume script
alias awsume="source \$(pyenv which awsume)"
alias ad="awsume default"

#Auto-Complete function for AWSume
#Auto-Complete function for AWSume
fpath=(~/.awsume/zsh-autocomplete/ $fpath)

