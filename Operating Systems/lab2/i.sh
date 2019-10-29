#!/bin/bash
# -r - recursive
# -s - supress warninss
# -h - remove file prefix
# -E - match regex
# -I - don't match binary
grep -rshI ACPI /var/log/* > errors.log

grep -E "(/[^/ ]*)+" errors.log
