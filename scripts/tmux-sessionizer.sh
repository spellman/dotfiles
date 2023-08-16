#! /bin/bash

if [[ $# -eq 1 ]]; then
    selected-dir=$1
else
  selected-dir=$(find ~/Projects/phylum/localdev ~/Projects/phylum/localdev/research ~/Projects/phylum ~/Projects ~/ ~/Documents ~/Projects/powerlifting-america -mindepth 1 -maxdepth 1 -type d | fzf)
fi

if [[ -z $selected-dir ]]; then
    exit 0
fi

selected_name=$(basename "$selected-dir" | tr . _)
is_tmux_running=$(pgrep tmux)

if [[ -z $TMUX ]] && [[ -z $is_tmux_running ]]; then
    tmux new-session -s $selected_name -c $selected-dir
    exit 0
fi

# -t is target session
if ! tmux has-session -t=$selected_name 2> /dev/null; then
    # -d is don't attach to the current terminal. (I think this prevents
    # nested tmux sessions.)
    # -s is session name
    # -c seems to be the working directory path, although that option is
    # absent from the man page. (It's a documented option for new-window
    # but not new-session.)
    tmux new-session -ds $selected_name -c $selected-dir
fi

tmux switch-client -t $selected_name
