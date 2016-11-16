NetRobots
=========

Robots fighthing in a virtual arena, and controlled by a REST API, so they can be written in any programming language, and they can be remote. 

The API is described in an human readable Swagger format in `doc/rest_api.yaml` file and `doc/README.md` file.

![screenshot](/doc/screenshot_1.jpg)

Status
======

Up to date it is in alpha/beta state. All the important features are implemented, but it is not well tested, and various refinements must be done. 

See `DEV.org` file for more info.

Installation
============

See `default.nix` or `Dockerfile` for a description of the required packages.

Starting
========

Unix
----

A demo server with demo clients can be launched in this way:

    cd demo_scripts
    ./run-demo.sh

The server can be launched manually using this params:

    python run.py

    options:

      --board-x-size=number            (default 1000)
      --board-y-size=number            (default 1000)
      --game-tick=number               The virtual simulation time in seconds,
                                       between two consecutive robot commands.
                                       (default 0.25)
      --network-latency=number         The network latency in seconds. (default 0.100)
      --run=port                       run the server on the specified http port

See `demo_scripts/run-demo.sh` for an example of how launching demo clients.

Using Docker
------------

TODO not yet completed/tested

Using Nix
---------

`default.nix` contains the required packages for the running environment.

The demo are launched in this way:

    cd demo_scripts
    nix-shell ../default.nix --run "./run-demo.sh"

The server can be launched in this way, from the base directory: 

    nix-shell default.nix --run "python run.py"

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

