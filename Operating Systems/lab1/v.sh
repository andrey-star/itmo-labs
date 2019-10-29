#!/bin/bash
echo "Enter 1 to open nano"
echo "Enter 2 to open vi"
echo "Enter 3 to open links"
echo "Enter 4 to exit"

while true
do
	read A
	case $A in
		1 )
		nano
		break
	;;
		2 )
		vi
		break
	;; 
		3 )
		links
		break
	;;
		4 )
		exit 0
	esac
	echo "Invalid command. Try again"
done