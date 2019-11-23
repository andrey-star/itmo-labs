#!/bin/bash

date=$(date +%F)
last_backup_date=$(find ~/Backup-* -maxdepth 0 2> /dev/null \
| sort -n | tail -n 1 | awk -F '/' '{print $4}' |\
	awk -F '-' '{print $2 "-" $3 "-" $4}')
if [[ -z $last_backup_date ]]; then
	day_diff=8
else
	sec_diff=$(($(date -d $date +%s) - $(date -d $last_backup_date +%s)))
	day_diff=$(($sec_diff/60/60/24))
fi

dir_name="Backup-"$date
if [[ $day_diff -gt 7 ]]; then
	echo "Creating new backup"
	mkdir ~/$dir_name
	cp -r ~/source/* ~/$dir_name
	echo "$date: Backup file created at $dir_name" >> ~/backup-report
	ls -R ~/source >> ~/backup-report
	echo -e "\n\n" >> ~/backup-report
	echo "Done"
else
	new_files=""
	renamed_files=""
	dir_name="Backup-"$last_backup_date
	echo "Updating backup"
	echo "$date: Backup file updated at $dir_name" >> ~/backup-report
	for file in $(find ~/source/*); do
		full_path=$file
		file=$(echo $file | sed "s+$HOME/source/++")
		dest=~/$dir_name/$file
		if [[ ! -d $full_path ]]; then
			if [[ ! -f $dest ]]; then
				cp ~/source/$file $dest
				new_files+=$file"\n"
			else
				cur_size=$(stat ~/source/$file -c%s)
				old_size=$(stat $dest -c%s)
				if [[ "$cur_size" -ne "$old_size" ]]; then
					mv $dest $dest"."$date
					cp ~/source/$file $dest
					renamed_files+="$file $file.$date\n"
				fi	
			fi
		else 
			mkdir $dest 2> /dev/null
		fi
	done
	echo -e "New files:\n"$new_files"\nUpdated files:\n"$renamed_files"\n\n" >> ~/backup-report
	echo "Done"
fi
