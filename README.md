# HOW TO CLONE

    git clone --recursive https://github.com/EPFL-LARA/leon-web

If you are juste upgrading, delete any folder named `leon` inisde `leon-web` and then do:

    git submodule update --init --recursive

# HOW TO RUN LEON ONLINE

## Configuration

Write conf/setup.conf and add the following lines, for example:

    http.port=9999
    app.url="http://localhost:9999/"

## Step 2 (to start leon-web)

    $ ./start.sh

## Troubleshooter

* *invalid build uri (no handler available)*

Make sure you have git version 1.9.5 installed so that after `git pull` there is a folder `leon-web/leon` which is not a symlink and which contains the leon project.

* *Other possible issues.*

You may have to remove the global sbt eclipse plugin if it interfere with the play framework.

## Notes for windows (64 bits)

You will need to clone the `leon` project as a subfolder of `leon-web`, since hard links are currently not supported on Windows.

Before running leon-web through `activator run`, follow the instructions [Leon for Windows](http://lara.epfl.ch/~mmayer/leon/index.html).
