
# HOW TO DEPLOY

Note that Leon-web listens to the port `9900` (see script `start.sh`), whereas `Leon-web-string` listens to `9876`, the only two ports opened to the external world. After providing your public key to the server, log in using the following command:

    ssh leonweb@laraserver2.epfl.ch

If it were for the first time, you would create a new session `tmux new -s SESSION_NAME`, but since it already exists, for now, just log in to the existing session `leon`:

    tmux att -dt leon

Now that you are in this session, you should be inside `~/git/leon-web` and the server should be running. Here are a few commands you can use:

**UPDATE: Easiest way**
This will make leon unavailable for a few minutes.

* `CTRL+d` to stop the server
* `git pull origin <branch_name>`
* `git submodule update --recursive`
* `./start.sh` (or `activator "run 9999"`)

**UPDATE: Nicer way**

* `CTRL+a,-` to create a new terminal (splitted) or `CTRL+a,c` to create a new one
* `git pull origin <branch_name>`
* `git submodule update --recursive`
* `activator stage` to recompile
* `exit` to exit this new terminal
* `CTRL+C` to exit the current running application. If it starts to recompile, no need to do the next step.
* `./start.sh` (or `activator "run 9999"`)

**Other TMUX commands**

* `CTRL+a,d` to detach
* `CTRL+a,c` Create another session
* `CTRL+a,n` Switch from one session to the other.
* `exit` to exit the session.
* `CTRL+a,-` New splitted terminal
* `CTRL+a,o` Switch from one splitted terminal to the other in the same window.
* `CTRL+a` and PAGUP, PAGEDOWN to view the history. `q` to exit this mode.

In the shell

* `tmux ls` to list sessions
