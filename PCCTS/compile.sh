#!/bin/bash
# Check parameters
file="${1%.*}"

if [ ! "$file" ]; then
  echo -e "Please enter a filename!\n"
  echo -e "Usage: compile.sh [<filename>|clean]\n"
  exit 1
fi

if [ $file == "clean" ]; then
  rm *.c *.dlg
  rm -rf output
  exit 0
fi

echo Compiling $1
echo 1 of 3
err=$( { antlr27 -gt $file.g; } 2>&1)
ret=$?
warn=$( echo "$err" | grep -q "warning" )
if [ $ret == "1" -o $? == "0"  ]; then
  echo -e "\nInterpretation error"
  echo -e "Error:\n$err" | tee
  exit 1
fi

echo 2 of 3
err=$( { dlg parser.dlg scan.c; } 2>&1)
if [ $? == "1" ]; then
  echo -e "\ndlg parsing error"
  echo -e "Error:\n$err" | tee
  exit 1
fi

echo 3 of 3
err=$( { g++ -o $file $file.c scan.c err.c; } 2>&1)
if [ $? == "1" ]; then
  echo -e "\nCompilation error"
  echo -e "Error:\n$err" | tee
  exit 1
fi
echo -e "Executing:\n"
./$file
