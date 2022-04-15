#!/bin/bash

if [ $# -lt 1 ]
then
	echo "Usage $0 subdirectory"
	exit 2
fi
cd $1

variableFile="variableFile.txt"
variableOut="variableOut.txt"
variables=`cat $variableFile | uniq | cut -d'=' -f2 | tr -d ' '`
initVariables="initVariables.txt"
cat /dev/null > $initVariables
for variable in $variables
do
    echo -n "variable = $variable" >> $initVariables
    # if 1st field matches the variable, then print that line
#    grep $variable $variableOut | awk -F= '$1 ~ /'"$variable"'/ {print $0}' | grep -v "IF" >> $initVariables 2>&1
    grep $variable $variableOut | grep -v "IF" | cut -d: -f2 | tr -d ' ' | sort | uniq | awk -F= -v awkvar=$variable '$1 ~ awkvar {print $0}' > tempfile.txt 2>&1
    # if initVar is not null, then echo it
    if [ -s tempfile.txt ]
    then
        echo "" >> $initVariables
        cat tempfile.txt >> $initVariables 
    else
#        echo " is not init or not used" >> $initVariables
#        echo " is not init or not used"
        notused=`grep $variable $variableOut | grep -v variable`
#		echo $notused >> $initVariables
		if [ -n "$notused" ]
		then
			echo " is not init" >> $initVariables
		else
			echo " is not used" >> $initVariables
		fi
    fi
    echo "" >> $initVariables
done

rm -f $variableFile $variableOut tempfile.txt outFile.txt
