# killall signal regex - if launch str matches regex - send signal
SIGNAL=$1
REGEX=$2
for pid in $(ps -axo pid | tail -n +2); do
  proc=$(readlink /proc/$pid/exe)
  if [[ $proc =~ $REGEX ]]; then 
  	echo $pid:$proc
  	echo "kill -$SIGNAL $pid"
    kill -$SIGNAL $pid
  fi
done
