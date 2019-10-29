A=""
C=""
while [[ "$C" != "q" ]]
do
	read C
	A=$A$C
done
echo $A
