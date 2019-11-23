#!/bin/bash
while true; do
  read line
  echo "$line" >> v.txt

  if [[ "$line" == "QUIT" ]]; then
  	echo "Quitting..."
    exit 0
  fi

  if [[ ! "$line" =~ [0-9]+ && $line != "+" && $line != "*" ]]; then 
	  echo "Incorrect input"
	  exit 1
  fi
    
done