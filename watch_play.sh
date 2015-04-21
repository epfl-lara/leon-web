#!/bin/sh
open=`netstat -tlunp 2>/dev/null| grep :9900 | wc -l`
if [ "$open" -eq 1 ]; then
    #test request
    curl --silent -f localhost:9900  -o /dev/null
    if [ "$?" -eq 0 ]; then
        echo "Process up and running.."
        exit 0;
    fi
fi

date
echo "Killing play..."
pidfile="/localhome/leonweb/git/leon-web/RUNNING_PID"
if [ -f "$pidfile" ]; then
    echo "I just restarted play" | mail -s "Leon web restarted" etienne.kneuss@epfl.ch
    echo "Killing " `cat $pidfile`
    kill -9 `cat $pidfile`
    rm $pidfile
else
    echo "PID file not found.."
fi
