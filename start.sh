PORT=`cat conf/setup.conf | grep http.port | cut -d= -f2`
if [ "$PORT" == "" ]; then
    PORT=9000
fi

./gen_version.sh

while [ /bin/true ]; do
    activator "run $PORT"
    sleep 5
    rm -f target/universal/stage/RUNNING_PID
done
