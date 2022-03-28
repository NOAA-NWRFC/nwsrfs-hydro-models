#!/bin/bash

cd ..
directories=`ls -d *`

#echo "$directories"

for directory in $directories
do
    if [ -d $directory ]
	then
		echo "$directory"
        commonblockinfo/FortranFiles.bash $directory/src
        commonblockinfo/getInitVariables.bash $directory/src
        mv $directory/src/initVariables.txt commonblockinfo/$directory.initVariables.txt
	fi
done
ls -lt commonblockinfo/*.txt
