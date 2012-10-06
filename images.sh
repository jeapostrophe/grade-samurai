#!/bin/sh

FILES=$(find . -name 'photo.jpg')

for i in ${FILES} ; do
    if file $i | grep image ; then
        ORIG=${i}.orig
        if [ -f ${ORIG} ] ; then
            du -h $i
        else       
            cp $i ${ORIG}
            convert ${ORIG} -resize x160 $i
            identify $i
        fi
    else
        rm $i
    fi
done
