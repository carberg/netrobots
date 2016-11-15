NetRobots
=========

Fighting robots controlled by code.

Up to date it is in alpha/development state. See `DEV.org` file for more info on application design, and missing features.

REST API
========

Robots are controlled by a REST API, so they can be written in any programming language, and fights between remote teams are supported.

The API is described in an human readable Swagger format in `doc/rest_api.yaml` file.

Code for different programming languages, is derived from this file, using the tools in `dev_scripts`.

In the `robot_examples` directory there are demo robots for different programming languages.

Installation
============

See `default.nix` or `Dockerfile` for a description of the required packages.

Starting
========

Using Docker
------------

TODO not yet completed/tested

Using Nix
---------

A demo server with demo clients can be launched in this way:

    cd demo_scripts
    nix-shell ../default.nix --run "./run-demo.sh"

The server can be launched in this way:

    nix-shell default.nix --run "python run.py"

    run.py options:

      --board-x-size                   (default 1000)
      --board-y-size                   (default 1000)
      --game-tick                      The virtual simulation time in seconds,
                                       between two consecutive robot commands.
                                       (default 0.25)
      --network-latency                The network latency in seconds. (default 0.100)
      --run                            run the server on the specified http port

See `demo_scripts/run-demo.sh` for an example of how launching demo clients.

Robots Coding Instructions
==========================

Robots can be written using any programming language, because they communicate with the server using REST API. More details on `doc/rest_api.yaml` and `robot_examples` directory.

Project Development
===================

See `DEV.org`.

Credits
=======

This is a fork of [https://github.com/gbinside/netrobots], using Tornado Python Web Server, and an improved REST API.

Freely inspired/based on P-ROBOTS [http://corewar.co.uk/probots/p-robo4.txt]

LICENSE
=======

GPLv3+

