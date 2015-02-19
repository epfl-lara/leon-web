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

if [ ! -d "$LEON_PATH" ]; then
    echo "$LEON_PATH does not exist"
    return;
fi

export _JAVA_OPTIONS="-Xmx4096m"

rm -rf leon
ln -sf $LEON_PATH leon

echo "Done."
