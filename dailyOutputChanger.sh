#!/bin/bash
# This simple script puts daily output variable codes located in $2 file into a directory of input files.
# WARNING! The input files have to have a same length.
# USAGE: path/of/this/file/dailyOutputChanger.sh path/to/the/ini/directory path/to/the/varList/file.
set -eou pipefail

INIDIR=$1
OUTCODES=$2
#-----------------
cd $INIDIR
HEADNUM=$(cat $(ls|head -n 1) | grep -n DAILY_OUTPUT | sed 's/:.*//g')
TAILNUM=$(( $(cat $(ls|head -n 1)|wc -l) - $(cat $(ls|head -n 1) | grep -n ANNUAL_OUTPUT| sed 's/:.*//g') + 2 ))

for iniFile in $(ls)
do
    echo "$(cat <(head -n $HEADNUM $iniFile) <(cat $OUTCODES) <(tail -n $TAILNUM $iniFile))" > $iniFile
done
