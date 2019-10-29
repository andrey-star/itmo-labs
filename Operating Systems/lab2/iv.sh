#!/bin/bash
find /bin/* | xargs -n 1 head -n 1 | grep -a "^#!" | sort | uniq -c \
| sort -nr | head -1 | awk '{print $2}' | tr -d "#!"
