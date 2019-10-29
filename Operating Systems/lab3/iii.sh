#!/bin/bash
REGEX="^/sbin/*"
for pid in $(ps -axo pid | tail -n +2); do
  proc=$(readlink /proc/$pid/exe)
  if [[ $proc =~ $REGEX ]]; then 
    echo $pid:$proc
  fi
done