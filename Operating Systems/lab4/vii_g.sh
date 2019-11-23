#!/bin/bash
while true; do
  read line
  case $line in
    TERM)
      kill -SIGTERM $(cat .vii)
      exit 0
      ;;
    +) 
      kill -USR1 $(cat .vii)
      ;;
    [*]) 
      kill -USR2 $(cat .vii)
      ;;
    *)
      :
      ;;
  esac
done