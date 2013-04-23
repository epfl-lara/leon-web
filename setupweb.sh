if [ $# -eq 0 ] ; then
    echo 'Usage: source ./setupweb.sh <path-to-leon>'
    exit 1
fi

LEON_PATH=$1

echo "Setting up environment for web..."
echo "LEON_PATH=$LEON_PATH"

if [ ! -d "$LEON_PATH" ]; then
    echo "$LEON_PATH does not exist"
    exit 1;
fi

if [ ! -f "$LEON_PATH/setupenv" ]; then
    echo "$LEON_PATH/setupenv does not exist"
    exit 1;
fi

source $LEON_PATH/setupenv


if [ ! -n "$SCALA_HOME" ]; then
    echo "\$SCALA_HOME not set!"
    exit 1;
fi

if [ ! -f "$LEON_PATH/target/scala-2.9.2/leon_2.9.2-2.0.jar" ]; then
    echo "Could not find leon package in: $LEON_PATH/target/scala-2.9.2/leon_2.9.2-2.0.jar"
    exit 1;
fi

if [ ! -f "$LEON_PATH/unmanaged/64/cafebabe_2.9.2-1.2.jar" ]; then
    echo "Could not find cafebabe package in: $LEON_PATH/unmanaged/64/cafebabe_2.9.2-1.2.jar"
    exit 1;
fi

if [ ! -f "$LEON_PATH/unmanaged/64/scalaz3.jar" ]; then
    echo "Could not find scalaz3 package in: $LEON_PATH/unmanaged/64/scalaz3.jar"
    exit 1;
fi

echo "Creating symlinks to external libs..."

ln -sf "$LEON_PATH/target/scala-2.9.2/leon_2.9.2-2.0.jar" lib/leon.jar
ln -sf "$LEON_PATH/unmanaged/64/scalaz3.jar"              lib/scalaz3.jar
ln -sf "$LEON_PATH/unmanaged/64/cafebabe_2.9.2-1.2.jar"   lib/cafebabe.jar

echo "Done."
