#!/bin/bash

make

echo "Running automatic tests..."

mkdir output

fs_in=`ls test/input*`

for f_in in $fs_in
do
	f_out=`echo $f_in | sed "s/input/output/g"`
	t_out=`echo output/$f_out | sed "s/test\///g"`

	cmd="./solver > /dev/null <<< $'$f_in\n$t_out'"

	echo -n "-> \"$f_in\" running... "
	eval $cmd

	if diff $t_out $f_out 2>&1 > /dev/null
	then
		echo -e "[\033[0;32mPASS\033[0m]"
	else
		echo -e "[\033[0;31mFAIL\033[0m]"
	fi
done

rm -rf output
