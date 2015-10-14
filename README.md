# HOW TO CLONE

    git clone --recursive https://github.com/EPFL-LARA/leon-web

If you are juste upgrading:

    git submodule update --init --recursive

Later, when pulling changes, use the command `git submodule update`.

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
    
Alternatively, `sbt run` may work. (or `sbt "run 9000"` to specify the port).

## Troubleshooter

* *invalid build uri (no handler available)*

Make sure you have git version 1.9.5 installed so that after `git pull` there is a folder `leon-web/leon` which is not a symlink and which contains the leon project.

* *Other possible issues.*

You may have to remove the global sbt eclipse plugin if it interfere with the play framework.

## Notes for windows

Before running leon-web through `activator run` or `sbt run`, follow the instructions [Leon for Windows](http://lara.epfl.ch/~mmayer/leon/index.html).

# HOW TO DEPLOY

Note that Leon-web listens to the port `9900` (see script `start.sh`), whereas Leon-scalajs listens to `9999`. After providing your public key to the server, log in using the following command:

    ssh leonweb@laraserver2.epfl.ch

If it were for the first time, you would create a new session `tmux new -s SESSION_NAME`, but since it already exists, for now, just log in to the existing session `leon`:

    tmux att -dt leon

Now that you are in this session, you should be inside `~/git/leon-web` and the server should be running. Here are a few commands you can use:

**UPDATE: Easiest way**
This will make leon unavailable for a few minutes.

* `CTRL+d` to stop the server
* `git pull origin scalajs`
* `./start.sh` (or `activator "run 9999"`)

**UPDATE: Nicer way**

* `CTRL+a,-` to create a new terminal (splitted) or `CTRL+a,c` to create a new one
* `git pull origin scalajs`
* `activator stage` to recompile
* `exit` to exit this new terminal
* `CTRL+C` to exit the current running application
* `./start.sh` (or `activator "run 9999"`)

**Other TMUX commands**

* `CTRL+a,d` to detach
* `CTRL+a,c` Create another session
* `CTRL+a,n` Switch from one session to the other.
* `exit` to exit the session.
* `CTRL+a,-` New splitted terminal
* `CTRL+a,o` Switch from one splitted terminal to the other in the same window.
* `CTRL+a` and PAGUP, PAGEDOWN to view the history. `q` to exit this mode.
