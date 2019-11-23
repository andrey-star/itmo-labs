#!/bin/bash

rm -rf ~/restore 2> /dev/null
mkdir ~/restore

backup_dir=$(find ~/Backup-* -maxdepth 0 | sort -n | tail -n 1)
echo "Restoring from $backup_dir"
for file in $(find $backup_dir/*); do
	full_path=$file
	file=$(echo $file | sed "s+$backup_dir/++")
	dest=~/restore/$file
	if [[ ! -d $full_path ]]; then
		if [ -z "$(echo $file | grep -E "*.[0-9]{4}-[0-9]{2}-[0-9]{2}$")" ];
		then
			cp $backup_dir"/"$file ~/restore/$file
		fi
	else
		mkdir $dest 2> /dev/null
	fi
done