#!/bin/sh
w=$(tmux list-windows -F '#{window_index} #{window_bell_flag}' | awk '$2==1{print $1; exit}')
if [ -n "$w" ]; then
  tmux select-window -t "$w"
else
  tmux display-message "No bell windows"
fi
