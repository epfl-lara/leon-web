HOWTO RUN LEON ONLINE
====================

  $ cd path/to/leon
  $ sbt package script
  $ cd path/to/leon-web
  $ source setupweb /path/to/leon


 **Make sure SCALA\_HOME is SET!**

On laraserver
------------

    $ export LD_LIBRARY_PATH=/localhome/leonweb/git/z3/build/
    $ play "start -Dapp.prefix=/leon"

Locally
------------

    $ play run
