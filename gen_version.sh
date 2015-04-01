#!/bin/bash

isdev=`pwd | grep -q "\\-dev"`;

isdev=$?

hash=`(cd leon; git log --date=short --format="%h") | head -n 1`
date=`(cd leon; git log --date=short --format="%ad") | head -n 1`

if [[ $isdev -eq 0 ]]; then
    echo "Leon-dev-$hash (Built $date)" > version
else
    echo "Leon-$hash (Built $date)" > version
fi
