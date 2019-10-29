if (($1 < $2))
then
	if (($2 < $3))
	then 
		echo $3
	else
		echo $2
	fi
else
	if (($1 < $3))
	then 
		echo $3
	else
		echo $1
	fi
fi
