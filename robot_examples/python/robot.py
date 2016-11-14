'''
    Copyright 2015 Roberto Gambuzzi <gbinside@gmail.com>
    Copyright 2015, 2016 Massimo Zaniboni <massimo.zaniboni@docmelody.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''

"""
Different types of robots that can be launched from the command line.
"""

# TODO import here the code of various robots
# TODO robots are started specifying some parameter from the command line

from rest_api.swagger_client.models import *
from rest_api.swagger_client import *

from math import atan2, degrees
from random import randint
import sys
import getopt

class Robot(object):
    """Base class for all robots.
    """

    _api_client = None   # type: DefaultApi
    _api_client_conf = None # type: ApiClient

    _cmd_scan = None # type: ScanCommand
    _cmd_drive = None # type: DriveCommand
    _cmd_fire = None # type: FireCommand

    _status = None # type: RobotStatus

    def __init__(self, url):
        """
        :type serverAddress: string
        :type serverPort: string
        """
        self._api_client_conf = ApiClient(url)
        self._api_client = DefaultApi(self._api_client_conf)

    def get_status(self):
        """
        :rtype: RobotStatus
        """
        return self._status

    def default_robot_configuration(self, robotName):
        """
        :type robotName: string
        :rtype: RobotConfiguration
        """
        r = RobotConfiguration()

        r.name = robotName
        r.max_hit_points = 100.0
        r.max_speed = 27.0  # m/s
        r.acceleration = 9.0  # m/s^2
        r.decelleration= -5.0  # m/s^2
        r.max_sterling_speed = 13.0
        r.max_scan_distance = 700.0
        r.max_fire_distance = 700.0
        r.bullet_speed = 500.0  # m/s
        r.bullet_damage = 10.0
        r.cannon_reloading_time = 1.0
        # NOTE: strenght is optimized from the server

        return r

    def create_robot(self, creationParams):
        """
        To call initially for creating the robot.
        :type creationParams: RobotConfiguration
        """
        self._status = self._api_client.robot_create_post(creationParams)

    def set_scan(self, v):
        """
        :type v: ScanCommand
        """
        self._cmd_scan = v

    def set_drive(self, v):
        """
        :type v: DriveCommand
        """
        self._cmd_drive = v

    def set_fire(self, v):
        """
        :type v: FireCommand
        """
        self._cmd_fire = v

    def send_cmd(self):
        """Send configured commands to the server, and wait for the answer.
        After the execution, the fire command will reset to None, but Drive and Scan will remain the same.
        The effects of the command are accessible using getStatus().
        """

        cmd = RobotCommand()
        cmd.token = self.get_status().token
        cmd.drive = self._cmd_drive
        cmd.scan = self._cmd_scan
        cmd.fire = self._cmd_fire
        self._status = self._api_client.robot_action_post(cmd)

        self._cmd_fire = None

class MonothreadRobot(Robot):
    """A basic monothread robot, executing one command at a time.
    """

    def distance(self, x0, y0, x1, y1):
        """
        :type x0: float
        :type y0: float
        :type x1: float
        :type y1: float
        :rtype: float
        """
        return ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** 0.5

    def goto(self, x, y):
        """Goto the specified position, and return when the position is reached.
        :type x: float
        :type y: float
        """

        dx = x - self.get_status().pos_x
        dy = y - self.get_status().pos_y
        heading = degrees(atan2(dy, dx))

        # Go to the point
        d = DriveCommand()
        d.speed = 100.0
        d.direction = heading

        self.set_drive(d)
        self.send_cmd()

        # Wait to be near the point
        d1 = self.distance(self.get_status().pos_x, self.get_status().pos_y, x, y)
        while d1 > 80.0:
            self.send_cmd()
            d1 = self.distance(self.get_status().pos_x, self.get_status().pos_y, x, y)

        # Speed down
        d.speed = 0
        self.set_drive(d)
        self.send_cmd()

        while self.get_status().speed > 0:
            self.send_cmd()

def run_robot_rabbit(serverAddress, robotName):
    robot = MonothreadRobot(serverAddress)
    conf = robot.default_robot_configuration(robotName)
    robot.create_robot(conf)

    maxX = robot.get_status().max_board_x
    maxY = robot.get_status().max_board_y
    borderX = maxX / 10.0
    borderY = maxY / 10.0
    maxX = maxX - borderX
    maxY = maxY - borderY

    isAlive = True
    while isAlive:
        if not robot.get_status().is_dead:
            robot.goto(float(randint(int(borderX), int(maxX))), float(randint(int(borderY), int(maxY))))


def run_robot_sniper(serverAddress, robotName):

    robot = MonothreadRobot(serverAddress)
    conf = robot.default_robot_configuration(robotName)
    robot.create_robot(conf)

    robot.goto(robot.get_status().max_board_x / 2.0, robot.get_status().max_board_y / 2.0)
    teta = 0
    resolution = 10
    while not robot.get_status().is_dead:
        sc = ScanCommand()
        sc.direction = teta
        sc.semiaperture = teta
        robot.set_scan(sc)
        robot.send_cmd()
        scan_distance = -1.0
        if robot.get_status().scan_status is not None:
            scan_distance = robot.get_status().scan_status.distance

        if scan_distance > -1.0 and scan_distance > 40.0:  # maximum damage radius
            if robot.get_status().cannon_reloading_time == 0.0:
                fc = FireCommand()
                fc.distance = robot.get_status().scan_status.distance
                fc.direction = teta
                robot.set_fire(fc)
                robot.send_cmd()
        else:
            teta += resolution * 2

class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg

def printUsage():
    print """

  python robot.py --help
  python robot.py [--server ADDRESS] --robot ROBOT-NAME

  default ADDRESS: http://127.0.0.1:80

Available ROBOT-NAME

  rabbit: simple robot, walking randomly in the board
  sniper: positions itself in the center of the board and it starts scanning circularly and firing to targets.

"""

def main(argv=None):
    if argv is None:
        argv = sys.argv

    try:
        try:
            opts, args = getopt.getopt(argv[1:], "h", ["help","server=", "robot="])

            serverAddress = 'http://127.0.0.1:80'
            robotName = None
            correctCmd = False

            for o, a in opts:
                if o in ("-h", "--help"):
                    correctCmd = True
                    printUsage()
                    return 0
                elif o in ("--server"):
                    serverAddress = a
                elif o in ("--robot"):
                    correctCmd = True
                    robotName = a
                else:
                    printUsage()
                    return 2

            if not correctCmd:
                printUsage()
                return 2

            if robotName is None:
                printUsage()
                return 2
            elif robotName in ("rabbit"):
                run_robot_rabbit(serverAddress, "rabbit")
                return 0
            elif robotName in ("sniper"):
                run_robot_sniper(serverAddress, "sniper")
                return 0
            else:
                printUsage()
                return 2

        except getopt.error, msg:
             raise Usage(msg)
        # more code, unchanged
    except Usage, err:
        print >>sys.stderr, err.msg
        print >>sys.stderr, "for help use --help"
        return 2

    return 0

if __name__ == "__main__":
    sys.exit(main())
