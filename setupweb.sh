if [ x"${BASH_SOURCE[0]}" == x"$0" ]; then
    echo 'Usage: source ./setupweb.sh <path-to-leon>'
    exit;
fi

if [ $# -eq 0 ] ; then
    echo 'Usage: source ./setupweb.sh <path-to-leon>'
    return;
fi

LEON_PATH=`readlink -m $1`

echo "Setting up environment for web..."
echo "LEON_PATH=$LEON_PATH"

if [ ! -d "$LEON_PATH" ]; then
    echo "$LEON_PATH does not exist"
    return;
fi

if [ ! -f "$LEON_PATH/setupenv" ]; then
    echo "$LEON_PATH/setupenv does not exist. Did you run 'sbt script' in leon project?"
    return;
fi

source $LEON_PATH/setupenv


if [ ! -n "$SCALA_HOME" ]; then
    echo "\$SCALA_HOME not set!"
    return;
fi

LEON_DEP="$LEON_PATH/target/scala-2.10/leon_2.10-2.0.jar"
SCALAZ3_DEP="$LEON_PATH/unmanaged/64/scalaz3_2.10-2.0.jar"
CAFEBABE_DEP="$LEON_PATH/unmanaged/64/cafebabe_2.10-1.2.jar"

if [ ! -f "$LEON_DEP" ]; then
    echo "Could not find leon package in: $LEON_DEP"
    return;
fi

if [ ! -f "$CAFEBABE_DEP" ]; then
    echo "Could not find cafebabe package in: $CAFEBABE_DEP"
    return;
fi

if [ ! -f "$SCALAZ3_DEP" ]; then
    echo "Could not find scalaz3 package in: $SCALAZ3_DEP"
    return;
fi

echo "Creating symlinks to external libs..."

mkdir -p lib

ln -sf "$LEON_DEP" lib/leon.jar
ln -sf "$SCALAZ3_DEP"              lib/scalaz3.jar
ln -sf "$CAFEBABE_DEP"   lib/cafebabe.jar

echo "Done."
