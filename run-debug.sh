
#
# Launch the server.
#

LOG_FILE=netrobots.log
KILL_FILE=kill-server.sh

python run.py --run=8888 2> $LOG_FILE &
echo "kill -8 $!" > $KILL_FILE
chmod u+x kill-server.sh
echo "Execute $KILL_FILE for killing the NetRobots server."
echo "Server logs are in $LOG_FILE"

#
# Launch the DEMO robots.
#
sleep 2
xdg-open http://localhost:8888 &
cd robot_examples/python
python robot.py --server http://localhost:8888 --robot rabbit &
