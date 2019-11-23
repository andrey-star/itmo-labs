#!/bin/bash
d=$(date +"%F_%R")
mkdir ~/test && echo "catalog test was created successfully" \
> ~/report.txt && touch ~/test/$d.txt

ping -c 1 www.net_nikogo.ru 2>> ~/report.txt