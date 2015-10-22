PORT=`cat conf/setup.conf | grep http.port | cut -d= -f2`
if [ "$PORT" == "" ]; then
    PORT=9000
fi

./gen_version.sh

JAVA_OPTS="-Xss100M -Xms2G -Xmx5G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled" \
activator "run $PORT"

