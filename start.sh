PORT=`cat conf/setup.conf | grep http.port | cut -d= -f2`
if [ "$PORT" == "" ]; then
    PORT=9000
fi

while [ /bin/true ]; do
    activator "start $PORT"
    sleep 5
done
