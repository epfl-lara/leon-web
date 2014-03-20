if [ x"${BASH_SOURCE[0]}" == x"$0" ]; then
    echo 'Usage: source ./setupweb.sh <path-to-leon>'
    exit;
fi

if [ $# -eq 0 ] ; then
    echo 'Usage: source ./setupweb.sh <path-to-leon>'
    return;
fi

LEON_PATH=`readlink -m $1`
LEON_VERSION="2.3"

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

rm -rf leon
ln -sf $LEON_PATH leon

echo "Done."
