if [ $HOME = $PWD ]
then
	echo $HOME
	exit 0
else
	echo "Error"
	exit 1
fi