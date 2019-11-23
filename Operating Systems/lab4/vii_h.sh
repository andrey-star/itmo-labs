#!/bin/bash
echo $$ > .vii
num=1
MODE="+"
term()
{
  MODE="TERM"
}
add()
{
  MODE="+"
}
mul()
{
  MODE="*"
}
trap 'term' SIGTERM
trap 'add' USR1
trap 'mul' USR2
while true; do
  case $MODE in
  	"+")
      num=$(($num + 2))
      ;;
    "*")
      num=$(($num * 2))
      ;;
    "TERM")
      echo "Stopped by SIGTERM"
      exit 0
      ;;
    *)
      :
  esac
  echo $num
  sleep 1
done

killall signal regex - launch str matches regex - send signal