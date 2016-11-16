#!/usr/bin/env bash

cd ..

# Run server with default configurations.

LOG_FILE=netrobots.log
KILL_FILE=kill-server.sh

python run.py --run=8888 2> demo_scripts/$LOG_FILE &
echo "kill -8 $!" > demo_scripts/$KILL_FILE
chmod u+x demo_scripts/kill-server.sh
echo "Execute $KILL_FILE for killing the NetRobots server."
echo "Server logs are in $LOG_FILE"

# Launch the demo robots.

sleep 2
xdg-open http://localhost:8888 &
cd robot_examples/python
python robot.py --server http://localhost:8888 --robot rabbit &
python robot.py --server http://localhost:8888 --robot rabbit &
python robot.py --server http://localhost:8888 --robot rabbit &
python robot.py --server http://localhost:8888 --robot sniper &
python robot.py --server http://localhost:8888 --robot rabbit &
python robot.py --server http://localhost:8888 --robot rabbit &
python robot.py --server http://localhost:8888 --robot rabbit &




