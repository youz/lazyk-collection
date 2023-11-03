#!/bin/sh

set -e

for test in test/*.lazy
do
    if [ -e ${test%.lazy}.in ]
    then
	$@ $test <${test%.lazy}.in |diff -u ${test%.lazy}.out -
    else
	$@ $test |diff -u ${test%.lazy}.out -
    fi
done

echo 'All tests passed'
