# HOW TO RUN LEON ONLINE

## Step 1

    $ cd path/to/leon
    $ sbt package script
    $ cd path/to/leon-web
    $ source setupweb.sh /path/to/leon

## Step 2

### On laraserver

    $ export LD_LIBRARY_PATH=/localhome/leonweb/git/z3/build/
    $ play "start -Dapp.prefix=/leon/ -Dapp.url=http://lara.epfl.ch"

### Locally

    $ play run
