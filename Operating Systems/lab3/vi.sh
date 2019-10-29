#!/bin/bash
prev_ppid=-1
sum=0
cnt=0
while read line; do
  ppid=$(echo ${line} | cut -d ":" -f2 | cut -d "=" -f2)
  sleepavg=$(echo ${line} | cut -d ":" -f3 | cut -d "=" -f2)
  if [[ ${prev_ppid} != -1 && ${prev_ppid} != ${ppid} ]]; then
    echo "Average_Sleeping_Children_of_ParentID="${prev_ppid}\
    "is" $(echo "${sum} / ${cnt}" | bc -l)
    sum=0
    cnt=0
  fi
  sum=$(echo ${sleepavg} + ${sum} | bc -l)
  ((cnt++))
  prev_ppid=${ppid}
  echo ${line}
done < "v.out"
echo "Average_Sleeping_Children_of_ParentID="${prev_ppid}\
"is" $(echo "${sum} / ${cnt}" | bc -l)