#!/bin/bash
for pid in $(ps axo pid | tail -n +2); do
  ppid=$(grep -s "PPid" "/proc/${pid}/status" | awk '{print $2}')
  sleepavg=0.${RANDOM}
  if [[ -n $ppid ]]; then
    echo "ProcessID="${pid}" : ParentProcessID="${ppid}\
    ": Average_Sleeping_Time="${sleepavg}
  fi
done | sort -nt "=" -k3 > "v.out"
