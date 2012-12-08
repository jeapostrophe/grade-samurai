#!/bin/sh

for user in users/* ; do
    prof_eval=${user}/assignments/final/prof-eval
    if [ -d ${prof_eval} ] ; then
        for i in ${prof_eval}/* ; do
            if grep -e '"" 0' $i > /dev/null ; then
                echo cp $i $(dirname $i)/../self-eval/$(basename $i)
            fi
        done
    fi
done
