#!/bin/bash

for i in `cat subrepos.txt`; do 
    url=$i
    name=`echo $i | sed 's/.*\/\(.*\)\.git/\1/'`
    git submodule add $i plugins/$name
done