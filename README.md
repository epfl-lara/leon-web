# HOW TO CLONE

    git clone --recursive https://github.com/EPFL-LARA/leon-web

If you are just upgrading, delete any folder named `leon` inside `leon-web` and then do

    git submodule update --init --recursive

Later, when pulling changes, use the command `git submodule update --recursive`. You might find the command `git submodule sync` useful when switching branches.

# HOW TO RUN A LEON SERVER ON LOCALHOST

## On linux (Ubuntu)

#### Install Typesafe's Activator

* Download the activator files from [here](https://www.typesafe.com/activator/download)
* Extract them somewhere on your computer
* Add the extracted folder to your PATH

#### Install Sphinx

[Sphinx webpage](http://www.sphinx-doc.org/en/stable/)

#### Create your configuration file

* Create the `localhome-leonweb/git/leon-web/` folders in your leon installation folder
* Create the `localhome-leonweb/repos` folder in your leon installation folder
* Create a `setup.conf`file in the `conf` folder of your leon installation folder and add the following lines (where `PATH_TO_LEON`is the path to your leon installation folder) :

	```
	# Web application settings
    app.ssl=false
    app.url="http://localhost:9000/"
    assets.production.external.dir="PATH_TO_LEON/localhome-leonweb/git/leon-web/"
    http.port=9000
    
    # Path to where the Git repositories will be cloned
    repositories.path="PATH_TO_LEON/localhome-leonweb/repos/"

    # GitHub application credentials
    auth.github.clientId="YOUR_GITHUB_APP_CLIENT_ID"
    auth.github.clientSecret="YOUR_GITHUB_APP_CLIENT_SECRET"```

    # Tequila application credentials
    auth.tequila.clientId="YOUR_TEQUILA_APP_CLIENT_ID"
    auth.tequila.clientSecret="YOUR_TEQUILA_APP_CLIENT_SECRET"
    
#### Launch your local leon server

Open a console in your leon installation folder run `activator run`.

#### Connect to your local leon server

You can now open a browser and go to `http://localhost:9000`. You should see the leon web ui.

# HOW TO RUN A SERVER FOR LEON

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

# Advanced running mode for leon-web (deploy, tmux, etc.)

Please [look into the wiki](https://github.com/epfl-lara/leon-web/wiki) for more advanced way of running leon-web
