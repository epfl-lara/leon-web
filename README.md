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

