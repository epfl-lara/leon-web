# HOW TO CLONE

    git clone --recursive https://github.com/EPFL-LARA/leon-web

If you are just upgrading, delete any folder named `leon` inside `leon-web` and then do

    git submodule update --init --recursive

Later, when pulling changes, use the command `git submodule update --recursive`. You might find the command `git submodule sync` useful when switching branches.

# HOW TO RUN LEON ONLINE

## Configuration

Write `conf/setup.conf` and add the following lines, for example:

    # Web application settings
    app.ssl=true
    app.url="https://leon.epfl.ch"
    assets.production.external.dir="/localhome/leonweb/git/leon-web/"
    http.port=9900
    
    # Path to where the Git repositories will be cloned
    repositories.path="/localhome/leonweb/repos/"

    # GitHub application credentials
    auth.github.clientId="YOUR_GITHUB_APP_CLIENT_ID"
    auth.github.clientSecret="YOUR_GITHUB_APP_CLIENT_SECRET"

## Step 2 (to start leon-web)

    $ ./start.sh

Alternatively, `sbt run` may work. (or `sbt "run 9000"` to specify the port).

## Troubleshooter

* *invalid build uri (no handler available)*

Make sure you have git version 1.9.5 installed so that after `git pull` there is a folder `leon-web/leon` which is not a symlink and which contains the leon project.

* *Other possible issues.*

You may have to remove the global sbt eclipse plugin if it interfere with the play framework.

## Notes for windows

Before running leon-web through `activator run` or `sbt run`, follow the instructions [Leon for Windows](http://lara.epfl.ch/~mmayer/leon/index.html).
