#!/bin/bash
awk -F ":" '{print $3 " " $1}' /etc/passwd | sort -n
