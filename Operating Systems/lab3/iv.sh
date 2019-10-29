#!/bin/bash
ps axo pid | tail -n +2 | \
awk '{printf $1 " "; system("cat /proc/"$1"/statm")}' 2> /dev/null \
| awk '{print $1 ":" $3 - $4}' | sort -nrt ":" -k2
