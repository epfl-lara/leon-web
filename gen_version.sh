#!/bin/bash

isdev=`pwd | grep "\\-dev"`;

leon=`(cd leon; git log --date=short --format="%h-%ad") | head -n 1`

if [[ $? == 0 ]]; then
    echo "leon-$leon-dev" > version
else
    echo "leon-$leon" > version
fi
