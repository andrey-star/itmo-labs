#!/bin/bash
touch v.txt

num=1
mode="+"
echo $num

(tail -n 0 -f v.txt) |
while true; do
  read line;
  case $line in
  	"+")
      mode="+"
      echo "Switching to adding mode"
      ;;
    "*")
      mode="*"
      echo "Switching to multiplying mode"
      ;;
    [0-9]*)
      case $mode in
      	"+")
          num=$(($num + $line))
          ;;
        "*")
          num=$(($num * $line))
          ;;
      esac
      echo $num
      ;;
    "QUIT")
      echo "Quitting..."
      killall tail
      exit 0
      ;;
    *)
      echo "Incorrect input"
      killall tail
      exit 1
      ;;
  esac
done