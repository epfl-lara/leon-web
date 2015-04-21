#!/bin/sh
while [ 0 ]; do
    open=`netstat -taunp 2>/dev/null| grep :9900 | wc -l`
    if [ "$open" -eq 0 ]; then
        exit ;
    fi
    echo "Still $open open sockets.."
    sleep 2;
done
