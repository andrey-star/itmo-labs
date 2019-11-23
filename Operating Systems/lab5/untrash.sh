#!/bin/bash

while read line; do
	if echo $line | grep -q "$1"; then
		trashed=$(echo $line | cut -d " " -f 1)
    	deleted=$(echo $line | cut -d " " -f 2-)
    	echo "Restore $deleted? (y/n):"
    	read resp < /dev/tty
    	if [[ "$resp" == "y" ]]; then
	        if [[ ! -d "$(dirname $deleted)" ]]; then
	        	echo "Previous directory not found. Restoring to home."
	        	deleted=~/$1
	        fi
	    	ln ~/.trash/$trashed "$deleted" &&
	        	rm ~/.trash/$trashed &&
	        	sed -i "/$line/d" ~/.trash.log
    		exit
    	fi
	fi
done < ~/.trash.log
