#!/bin/bash

if [ $# -lt 1 ]
then
	echo "Usage $0 subdirectory"
	exit 2
fi
subdirectory=$1
currentDirectory=`pwd`
#echo $subdirectory
cd $subdirectory

variableFile="variableFile.txt"
outFile="outFile.txt"
cat /dev/null > $variableFile
cat /dev/null > $outFile

fortranFiles1=`ls *.f`
fortranFiles2=`ls */*.f`
fortranFiles=`echo $fortranFiles1 $fortranFiles2`
#status=$?
#if [ $status -gt 0 ]
#then
#	fortranFiles=`ls *.f`
#fi

for file in $fortranFiles 
do
    echo "file = $file" >> $outFile
    variables=`$currentDirectory/commonblockinfo/searchCommonBlock.pl $file`
#    echo $variables
    for variable in `echo $variables`
    do
        echo "variable = $variable" >> $variableFile
    # grep a variable from a file and pipe to grep an equal sign
    # and pipe to grep a line which is not begin with 'C' nor 'c' and filtered out the lines with WRITE
        grep $variable $file | grep '=' | grep -v '^[Cc]' | grep -v "WRITE" >> $outFile
    # grep a variable from a file and pipe to grep a word DATA and pipe to grep a line which is not begin with 'C' nor 'c'
		grep $variable $file | grep "DATA" | grep -v '^[Cc]' >> $outFile
    done
    echo "" >> $outFile
done

variableOut="variableOut.txt"
cat /dev/null > $variableOut
variables=`cat $variableFile | uniq | cut -d'=' -f2 | tr -d ' '`
for variable in $variables
do
    echo "variable = $variable" >> $variableOut
    grep -n $variable $outFile >> $variableOut
    echo "" >> $variableOut
done
