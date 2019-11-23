#!/bin/bash

mkdir ~/.trash 2> /dev/null
cnt=$(ls ~/.trash | wc -l)
((cnt++))

ln "$1" ~/.trash/$cnt &&
	rm "$1" && 
	echo $cnt $(realpath "$1") >> ~/.trash.log