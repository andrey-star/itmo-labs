#!/bin/bash
echo $$ > .vi
cnt=0
MODE="RUN"
term()
{
  MODE="TERM"
}
trap 'term' SIGTERM
while true; do
  case $MODE in
    "RUN")
      ((cnt++))
      echo $cnt
      ;;
    "TERM")
      echo "Stopped by SIGTERM"
      exit 0
      ;;
  esac
  sleep 1
done
