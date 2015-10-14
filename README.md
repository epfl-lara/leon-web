# HOW TO RUN LEON ONLINE

## Step 1 (only once)

Setup link to leon from leon-web:

    $ cd path/to/leon-web
    $ source setupweb.sh /path/to/leon

Write conf/setup.conf and add the following lines, for example:

    http.port=9999
    app.url="http://localhost:9999/"


## Step 2 (to start leon-web)

You need typesafe activator, i.e. "activator" in the PATH. Then

    $ ./run.sh

## Notes for windows (64 bits)

You will need to clone the `leon` project as a subfolder of `leon-web`, since hard links are currently not supported on Windows.

Before running leon-web through `activator run`, follow the instructions [Leon for Windows](http://lara.epfl.ch/~mmayer/leon/index.html).