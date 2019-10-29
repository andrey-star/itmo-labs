#!/bin/bash
 grep -horsEI "[[:alnum:]._%+-]+@[[:alnum:].-]+(\.[a-zA-Z]+)+" /etc/* | sort \
 | uniq | tr "\n" "," | sed 's/,$/\n/' > emails.lst
cat emails.lst
