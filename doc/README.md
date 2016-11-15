Overview
========

Robots are put in a virtual arena, usually of 1000x1000. 

Coordinate system is:

* (0, 0) is the upper and left most point of the board
* (maxX, maxY) is the lowest and right most point of the board
* angles are expressed in degrees from 0 to 359
* 0 is the EAST direction, 90 is the SOUTH direction, 180 is the WEST direction, and 270 is the NORTH directon
* this coordinate system is usually default for graphics programming

Robots are created using a default configuration, or a customized one. Robot params can not surpass a certain threesold. Up to date I'm not sure of which params give an unfair advantage, so server code can be updated for not permitting too much strong robots.

Robots are created and commanded using a REST API. So they can be written in any programming language, and they can be remote respect the server. 

At every turn a robot can send three distinct types of commands: a scan command, a fire command and a drive command.

A fixed and configurable network delay is waited from the server before executing next commands.

The virtual simulation time is unrelated to the network delay: a robot live in a world where it can send a certain number of commands every virtual simulated second (default 4 commands by second). If the network delay is increased, then only the board viewer slow-down, but the virtual world simulation is not affected. A robot knows how many commands can issue every second, so its code can be parametrized according this.

The file `rest_api.yaml` contains all the details of the API.

