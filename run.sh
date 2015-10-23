PORT=`cat conf/setup.conf | grep http.port | cut -d= -f2`
if [ "$PORT" == "" ]; then
    PORT=9000
fi

./gen_version.sh

JAVA_OPTS="-Xss128M -Xms5G -Xmx15G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M" \
activator "run $PORT"

