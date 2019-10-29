#!/bin/bash
find /var/log 2> /dev/null | grep -sE "*\.log$" | xargs cat 2> /dev/null | wc -l
