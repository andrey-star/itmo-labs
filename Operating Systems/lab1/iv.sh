#!/bin/bash
C=1
read A
while (($A % 2))
do
	((C++))
	read A
done
echo $C