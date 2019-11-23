#!/bin/bash
while true; do
  read line
  case $line in
    TERM)
      kill -SIGTERM $(cat .vi)
      exit
      ;;
    *)
      :
      ;;
  esac
done