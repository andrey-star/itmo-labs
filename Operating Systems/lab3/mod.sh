for pid in $(ps -axo pid | tail -n +2); do
  proc=$(readlink /proc/$pid/exe)
  ln -s $proc $pid
done