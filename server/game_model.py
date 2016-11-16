'''
    Copyright 2015, 2016 Roberto Gambuzzi <gbinside@gmail.com>
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
Simulate the game.
"""

import hashlib
from math import cos, sin, radians, atan2, degrees, pi
from random import randint
import time
from server.rest_api.models import RobotStatus, RobotCommand, RobotConfiguration, RobotInfo, ScanStatus
from server.rest_api.models import EventCreateRobot, EventDrive, EventExplosion, EventMissile
from server.rest_api.models import EventRemoveRobot, EventRobotCollision, EventScan
from server.rest_api.models import Event
import uuid
from math import sqrt
import sys
from six import iteritems

from datetime import datetime
from datetime import date

class Board:
    """The Game Model.
    """

    def __init__(self, size=(1000, 1000), turn_delta_time = 1.0, network_latency = 0.1):

        self._global_time = 0.0
        self._next_turn_time = 0.0
        self._size = size
        self._robots_by_token = {}
        self._missiles = {}
        self._explosions = {}
        self._wall_hit_damage = 2
        self._join_status = None
        self._turn_delta_time = turn_delta_time
        self._network_latency = network_latency
        self._simulation_tick = self.get_optimal_simulation_tick()
        self._next_robot_id = 0
        self._robot_color = [
            'black', 'blue', 'green', 'red', 'gray', 'BlueViolet', 'Chocolate'
            , 'DarkCyan', 'DarkGreen', 'Khaki', 'Magenta', 'MediumSeaGreen', 'YellowGreen']

        self._radar = {}

        self.reset_streamed_events()

        if self._simulation_tick > self._turn_delta_time:
            self._simulation_tick = self._turn_delta_time / 10

    _simulation_tick = 0.125

    _turn_delta_time = 1.0

    _network_latency = 0.1

    _next_robot_id = 0

    _robot_color = []

    def get_optimal_simulation_tick(self):
        """
        :return: a simulation tick that can take in account precision of fast objects
        """

        # Use as value the speed of a missile, and consider its prediction respect a robot
        # must be precise within the 1.0 position, for determining the correct damage.
        # It must be also a multiple of self._turn_delta_time because every few simulation ticks
        # there should be an application of a command.

        t1 = 1.0 / 500.0
        i1 = self._turn_delta_time / t1
        i2 = round(i1) * 1.0
        t2 = self._turn_delta_time / i2
        return t2

    def get_turn_delta_time(self):
        """How many simulation seconds there are between robot commands.
        :rtype: Float
        """
        return self._turn_delta_time

    def get_network_latency(self):
        """How many seconds there are between network commands.
        :rtype: Float
        """
        return self._network_latency

    def get_simulation_tick(self):
        """An inner param, about the tick used for simulating the board. Less is this value, more accurate is the simulation, but slower are the calcs.
        """
        return self._simulation_tick

    def get_simulation_time_inside_realtime(self, realtime):
        """
        :type realtime: Float
        :rtype: Float
        :return: the simulation time corresponding to realtime
        """

        return (self.get_turn_delta_time() / self.get_network_latency()) * realtime

    _streamed_events = []

    _streamed_events_start_time = 0.0

    def reset_streamed_events(self):
        self._streamed_events = []
        self._stream_events_start_time = self.global_time()

    def get_streamed_events_start_time(self):
        return self._stream_events_start_time

    def get_streamed_events(self):
        """
        :rtype: [Event]
        :return:
        """
        return self._streamed_events

    def get_current_status(self):
        """
        :rtype: [Event]
        :return: the status of all robots on the board
        NOTE: the status of the missiles is missing, but this function is used only for visualization
        and it is acceptable missing the missiles in the first display frame.
        The status will be correct within few simulation passages.
        """

        r = []
        for robot in self._robots_by_token.values():
            if not robot.is_dead:
                e = EventCreateRobot()
                e.event_type = 1
                e.name = robot.name
                e.activation_time = robot.simulation_time
                e.color = robot.get_color()
                e.robot = robot.get_robot_info()
                r.append(e)

        return r

    def add_streamed_event(self, event):
        self._streamed_events.append(event)

    def get_size_x(self):
        return self._size[0]

    def get_size_y(self):
        return self._size[1]

    def get_missiles(self):
        """
        :rtype: dict[string, Missile]
        """
        return self._missiles

    def global_time(self):
        return self._global_time

    def robot_name_already_exists(self, robot_name):
        """

        :param robot_name:
        :type robot_name: bool
        :return: True if the robot name already exists
        :rtype: bool
        """

        for r in self._robots_by_token.itervalues():
            """:var r: Robot"""
            if r.name == robot_name:
                return True

        return False

    def _update_robot_time(self, robot):
        robot.simulation_time = self._global_time
        robot.time_tick = self._turn_delta_time
        robot.real_time_tick = self._network_latency

    def simulate_for_a_turn(self):
        """Simulate the board for a turn.
        """

        # Remove dead robots.
        # This is the right moment, because dead robots must not influence next computation phases,
        # and they must disappear from the board, after their dead was notified in previous turn.
        robots_to_remove = []
        for robot in self._robots_by_token.values():
            if robot.is_dead:
                robots_to_remove.append(robot)

                # Manage BoardViewer Events
                e = EventRemoveRobot()
                e.activation_time = self.global_time()
                e.event_type = 2
                e.robot = robot.get_robot_info()
                self._streamed_events.append(e)

        for robot in robots_to_remove:
            self.remove_robot_by_token(robot.token)

        # Fire and apply move command.
        # In this stage the fire commands are more successfully,
        # because based on the last board status.
        for robot in self._robots_by_token.values():
            if robot.get_next_command() is not None:
                fire = robot.get_next_command().fire
                ":type: FireCommand"
                if fire is None:
                    robot.no_fire()
                else:
                    robot.fire(fire.direction, fire.distance)

                    # Manage BoardViewer Events
                    e = EventMissile()
                    e.activation_time = self.global_time()
                    e.event_type = 4
                    e.robot = robot.get_robot_info()
                    e.direction = fire.direction
                    e.distance = fire.distance
                    e.speed = robot.configuration.bullet_speed
                    self._streamed_events.append(e)

                drive = robot.get_next_command().drive
                ":type: DriveCommand"
                if drive is not None:
                    robot.drive(drive.direction, drive.speed)

                    # Manage BoardViewer Events
                    e = EventDrive()
                    e.activation_time = self.global_time()
                    e.event_type = 7
                    e.robot = robot.get_robot_info()
                    self._streamed_events.append(e)

        # Simulate the movements.
        tick = self.get_simulation_tick()
        remaining_time = self.get_turn_delta_time()
        while remaining_time > 0:
            if remaining_time >= tick:
                remaining_time = remaining_time - tick
            else:
                tick = remaining_time
                remaining_time = 0
            self.tick(tick)

        # Apply scan commands, because it is the moment when the scan
        # is more precise.
        # Update also simulation_time for each robot.
        for robot in self._robots_by_token.values():
            robot.scan_status = None
            if robot.get_next_command() is not None and not robot.is_dead:
                scan = robot.get_next_command().scan
                ":type: ScanCommand"
                if scan is not None:
                    robot.scan(scan.direction, scan.semiaperture)

                    # Manage BoardViewer Events
                    e = EventScan()
                    e.activation_time = self.global_time()
                    e.event_type = 3
                    e.direction = scan.direction
                    e.semiaperture = scan.semiaperture
                    e.scan_max_distance = robot.configuration.max_scan_distance
                    e.robot = robot.get_robot_info()
                    if robot.get_scan_hit_robot() is None:
                        e.hit_robot = None
                    else:
                        e.hit_robot = robot.get_scan_hit_robot().get_robot_info()
                    self._streamed_events.append(e)

            self._update_robot_time(robot)
            robot.reset_command()

    def create_robot(self, configurations):
        """
        Create a new robot with some configurations, and add to the current board.
        Return an inactive status in case of problems.

        :type configurations: RobotConfiguration
        :type timeTick: float
        :type realTimeTick: float
        :rtype: Robot
        """

        # Use a unique name inside the system
        original_name = configurations.name
        name = original_name
        c = 1
        while self.robot_name_already_exists(name):
            c = c + 1
            name = original_name + "_" + str(c)

        self._next_robot_id = self._next_robot_id + 1
        configurations.name = name
        color = self._robot_color[self._next_robot_id % len(self._robot_color)]
        robot = Robot(name, str(uuid.uuid4()), self._next_robot_id, color, self, configuration=configurations)
        self._update_robot_time(robot)
        self.add_robot(robot)

        # Manage BoardViewer Events
        e = EventCreateRobot()
        e.activation_time = self.global_time()
        e.event_type = 1
        e.name = robot.name
        e.color = robot.get_color()
        e.robot = robot.get_robot_info()
        self._streamed_events.append(e)
        return robot

    def add_robot(self, robot):
        """ Add the robot inside the board.
        :type robot: Robot
        """

        self._robots_by_token[robot.token] = robot
        robot.last_command_executed_at_global_time = self.global_time()

    def remove_robot_by_token(self, token):
        """
        :type token: str
        :rtype: bool
        """
        if token in self._robots_by_token:
            del self._robots_by_token[token]
            return True
        else:
            return False

    def get_robot_by_token(self, token):
        """
        :rtype token: int
        :rtype: Robot
        """
        if token in self._robots_by_token:
            return self._robots_by_token[token]
        else:
            return None

    def reinit(self, size=(1000, 1000)):
        """Reinit. Used in debug. """
        self.__init__(size)

    def radar(self, scanning_robot, max_scan_distance, degree, semiaperture):
        """
        :type scanning_robot: Robot
        :type max_scan_distance: float
        :type degree: float
        :type semiaperture: float
        :rtype: (float, Robot)

        :return: (-1.0, None) if there is no found object, the object with minimum distance otherwise.
        """

        degree %= 360.0
        semiaperture %= 360.0

        start_degree = (degree - semiaperture) % 360.0
        end_degree = (degree + semiaperture) % 360.0
        angle_cross_zero = (start_degree > end_degree)

        found_distance = -1.0
        found_robot = None
        for robot in self._robots_by_token.values():
            """:type robot: Robot"""

            if robot == scanning_robot:
                continue

            if robot.is_dead:
                continue

            distance, angle = scanning_robot.distance(robot)

            if angle_cross_zero:
                inside = (angle >= start_degree and angle <= 360.0) or (angle <= end_degree)
            else:
                inside = (angle >= start_degree and angle <= end_degree)

            if not inside:
                continue

            if distance > max_scan_distance:
                continue

            if found_robot is None or distance <= found_distance:
                found_distance = distance
                found_robot = robot

        return found_distance, found_robot

    def detect_collision(self, robot, dxy):
        """
        :type robot: Robot
        :type dxy: (float, float)
        :rtype: (float, float, float)
        :return: x, y, damage to apply, or None for no collision
        """

        x, y = robot.get_xy()
        dx, dy = dxy

        # collision with other robots
        step = max(abs(dx), abs(dy))
        if step:  # the robot is moving
            x0, y0 = x - dx, y - dy
            stepx = float(dx) / float(step)
            stepy = float(dy) / float(step)
            for other_robot in [j for j in self._robots_by_token.values() if j.token != robot.token]:
                """:type other_robot: Robot """
                xp, yp = x0, y0
                for i in xrange(int(step) + 1):
                    dist, angle = other_robot.distance_from_point((xp, yp))
                    if dist < 2.0:  # 1 per ogni robot
                        other_robot.take_damage(self._wall_hit_damage)
                        other_robot.block()
                        teta = atan2(dy, dx)
                        xp, yp = other_robot.get_xy()
                        xp -= 2 * cos(teta)
                        yp -= 2 * sin(teta)
                        return xp, yp, self._wall_hit_damage
                    xp += stepx
                    yp += stepy

        # COLLISION WITH WALLS
        if x < 0:
            y = y - dy * x / dx
            x = 0
            return x, y, self._wall_hit_damage
        if x > self._size[0]:
            y = y - dy * (x - self._size[0]) / dx
            x = self._size[0]
            return x, y, self._wall_hit_damage
        if y < 0:
            x = x - dx * y / dy
            y = 0
            return x, y, self._wall_hit_damage
        if y > self._size[1]:
            x = x - dx * (y - self._size[1]) / dy
            y = self._size[1]
            return x, y, self._wall_hit_damage
        return None

    def spawn_missile(self, xy, degree, distance, bullet_speed, bullet_damage, owner):
        """
        :type xy: (float, float)
        :type degree: float
        :type distance: float
        :type bullet_speed: float
        :type bullet_damage: float
        :type owner: Robot
        """

        key = uuid.uuid4()
        missile = Missile(self, key, xy, degree, distance, bullet_speed, bullet_damage, owner)
        self._missiles[key] = missile

    def remove_missile(self, missile):
        """
        :type missile: Missile
        """
        del self._missiles[missile.key()]

    def tick(self, deltatime):
        self._global_time = self._global_time + deltatime

        # manage robots, before missile and explosions,
        # because they must be where the explosion can be at the tick time.
        for r in self._robots_by_token.itervalues():
            if not r.is_dead:
                r.tick(deltatime)

        # manage missiles
        missiles_to_remove = []
        for m in self._missiles.values():
            assert isinstance(m, Missile)

            if m.is_active():
                m.tick(deltatime)

            if not m.is_active():
                missiles_to_remove.append(m)

        # remove missiles without bad interactions with the original list
        for m in missiles_to_remove:
            self.remove_missile(m)

class Missile:
    """
    A balistic missile with a precise point of explosion.
    """

    def __init__(self, board, key, xy, degree, distance, speed, damage, owner):
        """
        :type board: Board
        :type key: str
        :type xy: (float, float)
        :type degree: float
        :type distance: float
        :type speed: float
        :type damage: float
        :type owner: Robot
        """
        self._key = key
        self._board = board
        self._owner = owner
        self._x, self._y = xy
        self._degree = degree
        self._target_distance = distance
        self._remaining_distance = distance
        self._speed = speed
        self._damage = damage
        self._space = 0.0
        self._is_active = True
        self._dst_x = self._x + self._target_distance * cos(radians(self._degree))
        self._dst_y = self._y + self._target_distance * sin(radians(self._degree))

    def key(self):
        return self._key

    def is_active(self):
        return self._is_active

    def tick(self, deltatime):
        deltadist = self._speed * deltatime
        self._remaining_distance -= deltadist
        next_distance = self._remaining_distance - deltadist

        if next_distance < 0.0:
            # NOTE: this tick is the most precise moment in which the missile reached the target.

            self._is_active = False

            for robot in self._board._robots_by_token.values():
                if not robot.is_dead:
                    distance, angle = robot.distance_from_point((self._dst_x, self._dst_y))
                    total_damage = self.get_damage(distance)
                    if total_damage > 0.0:
                        robot.take_damage(total_damage)
                        self._owner.points += total_damage

                        # Manage BoardViewer Events
                        e = EventExplosion()
                        e.activation_time = self._board.global_time()
                        e.event_type = 5
                        e.robot = self._owner.get_robot_info()
                        e.hit_robot = robot.get_robot_info()
                        e.damage = total_damage
                        self._board.add_streamed_event(e)

    def get_max_damage(self):
        """
        :return: the maximum damage of the explosio
        :rtype: float
        """
        return self._damage

    def get_perfect_damage_distance(self):
        """
        :return: the distance within an explosion hit perfectly the target, and it is doing 100% of the damage.
        This is near 1, and it is an estimation of robot and missile dimensions.
        :rtype: float
        """
        return 2.0

    def get_max_damage_distance(self):
        """
        :return: the distance after an explosion does 0 damage.
        :rtype: float
        """""
        return 45.0

    def get_initial_relative_damage_of_near_explosion(self):
        """
        :return: the perc of damage of an explosion not hitting the enemy robot. From 0 to 1.
        :rtype: float
        """
        return 0.5

    def get_damage(self, distance):
        """
        :param distance:
        :type distance: float

        :return: the damage according the distance of the target from the explosion.
        :rtype: float
        """

        if distance <= self.get_perfect_damage_distance():
            return self._damage

        if distance >= self.get_max_damage_distance():
            return 0.0

        # calculate a proportional damage
        distance2 = distance - self.get_perfect_damage_distance()
        max_distance2 = self.get_max_damage_distance() - self.get_perfect_damage_distance()
        relative_distance = 1.0 - distance2 / max_distance2

        return self.get_max_damage() * self.get_initial_relative_damage_of_near_explosion() * relative_distance

class Robot(RobotStatus):
    """ A robot in the board.
    """

    def __init__(self, name, token, robot_id, color, board, configuration=None):
        """
        :param name: the name of the robot
        :param token: the unique token
        :param configuration: None for using default configurations. A configuration to use otherwise.

        :type name: str
        :type token: str
        :type robot_id: int
        :type board: Board
        :type configuration: RobotConfiguration
        """
        super(Robot, self).__init__()

        self.name = name
        self.token = token
        self.is_dead = False
        self.speed = 0.0
        self.points = 0.0
        self.direction = float(randint(0,359))
        self.fired_new_missile = False
        self.cannon_reloading_time = 0.0
        self.max_board_x = board.get_size_x()
        self.max_board_y = board.get_size_y()
        self.missed_turns = 0

        self._last_command_executed_at_global_time = 0.0
        self._required_speed = 0.0
        self._board = board
        self._next_command = None
        self._robot_id  = robot_id
        self._scan_hit_robot = None

        self._color = color

        # proportional border
        border_x= self.max_board_x / 10.0
        border_y = self.max_board_y / 10.0

        # Find an initial position without collisions
        free_position = False
        while not free_position:
            self._pos_x, self._pos_y = float(randint(border_x, self.max_board_x - border_x)), float(randint(border_y, self.max_board_y - border_y))
            collision = self._board.detect_collision(self, (1, 1))
            if collision is None:
                free_position = True

        if configuration is None:
            self.set_to_default_configured_strength()
        else:
            self._configuration = configuration
            self.normalize_to_valid_strength()

        self._health = self._configuration.max_hit_points

        if self.get_configured_strength() > self.max_configurable_strength():
            self._is_dead = True

    def get_robot_id(self):
        """
        :rtype: int
        """
        return self._robot_id

    def get_color(self):
        return self._color

    def set_next_command(self, command):
        """
        Set the command, and register the robot using a new token.

        :type command: RobotCommand
        """
        self._next_command = command
        self._board.remove_robot_by_token(self.token)
        self.token = str(uuid.uuid4())
        self._board.add_robot(self)

    def reset_command(self):
        self._next_command = None

    def get_next_command(self):
        """
        :rtype: RobotCommand
        """
        return self._next_command

    def max_configurable_strength(self):
        """
        :rtype: float
        :return: the maximum configurable strength of the robot,
        that is an estimate of all the characteristicts of the robot like speed, health, acceleration and so on
        """
        return 327

    def get_maximum_configurable_reloading_time(self):
        """
        :rtype: float
        :return: the maximum time a robot can reload the fire.
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 6.0

    def get_minimum_configurable_reloading_time(self):
        """
        :rtype: float
        :return: the minimum time a robot can reload the fire
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 2.0

    def get_minimum_configurable_bullet_damage(self):
        """
        :rtype: float
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 5.0

    def get_maximum_configurable_bullet_damage(self):
        """
        :rtype: float
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 20.0

    def get_configured_strength(self):
        """
        :rtype: float
        :return: from 0 to max_configurable_strength or higher, for a measure of the strength of the robot,
        according its speed, acceleration, health and so on.
        TODO adapt this function with experience to a balanced setting
        """

        fires_for_second = 1.0 / self._configuration.cannon_reloading_time

        return sum([
            self._configuration.max_hit_points,
            2.0 * self._configuration.max_speed,
            2.0 * self._configuration.acceleration,
            -2.0 * self._configuration.decelleration,
            2.0 * self._configuration.max_sterling_speed,
            0.01 * self._configuration.max_scan_distance,
            0.01 * self._configuration.max_fire_distance,
            0.01 * self._configuration.bullet_speed,
            self._configuration.bullet_damage * 10.0 * fires_for_second,
        ])

    def set_to_default_configured_strength(self):
        """
        Configure the robot to its default configurations.

        IMPORTANT: change the REST API description in case of modification of these values.
        """

        self._configuration = RobotConfiguration()

        self._configuration.max_hit_points = 100.0
        self._configuration.max_speed = 27.0  # m/s
        self._configuration.acceleration = 9.0  # m/s^2
        self._configuration.decelleration = -5.0  # m/s^2
        self._configuration.max_sterling_speed = 13.0
        self._configuration.max_scan_distance = 700.0
        self._configuration.max_fire_distance = 700.0
        self._configuration.bullet_speed = 500.0  # m/s
        self._configuration.bullet_damage = 10.0
        self._configuration.cannon_reloading_time = 2.0

        self.normalize_to_valid_strength()

    def configured_strength_respect_max_configurable(self):
        """
        :return: 1 if the robot is using 100% of maximum configurable strength,
        0 if it is nothing.
        :rtype: float
        """

        return self.configured_strength() / self.max_configurable_strength()

    def normalize_to_valid_strength(self):
        """
        Reduce or increase strength to a valid value.
        Up to date the only effect is maximizing or minimizing the bullet damage.
        """

        self._configuration.cannon_reloading_time = min(self._configuration.cannon_reloading_time, self.get_maximum_configurable_reloading_time())
        self._configuration.cannon_reloading_time = max(self._configuration.cannon_reloading_time, self.get_minimum_configurable_reloading_time())

        if self.get_configured_strength() > self.max_configurable_strength():
            self.reduce_to_valid_strength()
        else:
            self.maximize_configured_strength()

        self._configuration.bullet_damage = min(self._configuration.bullet_damage, self.get_maximum_configurable_bullet_damage())
        self._configuration.bullet_damage = max(self._configuration.bullet_damage, self.get_minimum_configurable_bullet_damage())

    def reduce_to_valid_strength(self):
        """
        Reduce the strength to a valid value.
        Up to date the only effect is reducing the bullet damage.

        If the robot is too much strong, then the bullet damage became 0.

        :return:
        """
        bullet_damage = self._configuration.bullet_damage
        delta = 0.5
        while self.get_configured_strength() > self.max_configurable_strength() and bullet_damage >= self.get_minimum_configurable_bullet_damage():
            bullet_damage = bullet_damage - delta
            self._configuration.bullet_damage = bullet_damage

        if self._configuration.bullet_damage < 0:
            self._configuration.bullet_damage = 0


    def maximize_configured_strength(self):
        """
        Change the current configured_strength for reaching the maximum available.
        Up to date the only effect is in maximizing the bullet damage
        """
        bullet_damage = self._configuration.bullet_damage
        delta = 0.5
        while self.get_configured_strength() <= self.max_configurable_strength() and bullet_damage <= self.get_maximum_configurable_bullet_damage():
            bullet_damage = bullet_damage + delta
            self._configuration.bullet_damage = bullet_damage

        self.reduce_to_valid_strength()

    def get_robot_info(self):
        """
        :rtype: RobotInfo
        """
        r = RobotInfo()
        r.robot_id = self._robot_id
        r.pos_x = self.pos_x
        r.pos_y = self.pos_y
        r.direction = self.direction
        r.current_speed = self.speed
        r.required_speed = self._required_speed
        if self._required_speed > self.speed:
            r.acceleration = self.configuration.acceleration
        else:
            r.acceleration = self.configuration.decelleration
        r.reloading_time = self.cannon_reloading_time
        r.health = self.health
        r.points = self.points
        r.missed_turns = self.missed_turns

        return r

    def drive(self, degree, speed):
        """
        Change the direction and speed of the robot.
        :type degree: float
        :type speed: float
        """
        degree = float(int(degree) % 360)
        if degree != self.direction and self.speed > self.configuration.max_sterling_speed:
            # decellerate without changing direction
            self._required_speed = 0.0
        else:
            self.direction = degree
            self._required_speed = min(speed, self.configuration.max_speed)

    def scan(self, direction, semiApertureAngle):
        """
        Activate a scan command.
        The result is returned in `scan_status` property.
        :type direction: float
        :type semiApertureAngle: float
        """

        if semiApertureAngle <= 0.0:
            semiApertureAngle = 1.0

        semiApertureAngle %= 180.0
        direction %= 360.0

        distance, foundRobot = self._board.radar(self, self._configuration.max_scan_distance, direction, semiApertureAngle)

        scan = ScanStatus()
        scan.direction = direction
        scan.semi_aperture_angle = semiApertureAngle
        scan.distance = distance
        self.scan_status = scan
        self._scan_hit_robot = foundRobot

    def get_scan_hit_robot(self):
        return self._scan_hit_robot

    def fire(self, degree, distance):
        """
        Activate cannon command if the robot can fire a missile. Nothing otherwise.
        :type degree: float
        :type distance: float
        :param degree:
        :param distance:
        """
        if self.cannon_reloading_time == 0.0:
            degree, distance = float(int(degree) % 360), min(distance, self._configuration.max_fire_distance)

            self._board.spawn_missile(
                (self._pos_x, self._pos_y),
                degree,
                distance,
                self._configuration.bullet_speed,
                self._configuration.bullet_damage,
                self)

            self.cannon_reloading_time = self._configuration.cannon_reloading_time
            self.fired_new_missile = True
        else:
            self._fired_new_missile = False

    def no_fire(self):
        """To call if the robot has not fired any missile in this turn."""
        self._fired_new_missile = False

    def distance_from_point(self, xy):
        """
        :type xy: (float, float)
        :rtype: float
        :param xy: point
        :rtype: (float, float)
        :return: distance, angle The angle is always normalized from 0.0 to 360.0
        """

        dx = (xy[0] - self._pos_x)
        dy = (xy[1] - self._pos_y)

        if dx == dy:
            return 0.0, 0.0

        dist = sqrt(dx ** 2 + dy ** 2)
        rads = atan2(dy, dx)
        angle = float(degrees(rads)) % 360.0
        return dist, angle

    def distance(self, enemy):
        """
        :type enemy: Robot
        :rtype: (float, float)
        :return: distance, angle
        """
        return self.distance_from_point((enemy.pos_x, enemy.pos_y))

    def block(self):
        """ Block immediately the robot.
        Call only in case of death or hit of a wall, because immediate block without deceleration can be cheating...
        """
        self._speed = 0.0
        self._required_speed = 0.0

    def tick(self, deltatime):
        """Update the status of the robot for a simulation tick,
        involving only movements.

        :type deltatime: float
        """

        self.simulation_time = self.simulation_time + deltatime

        # RELOADING
        if self.cannon_reloading_time > 0.0:
            self.cannon_reloading_time -= deltatime
            if self.cannon_reloading_time <= 0.0:
                self.cannon_reloading_time = 0.0

        # SPEED calculation with accelleration/decelleration limit
        if self.speed < self._required_speed:
            acceleration = self.configuration.acceleration
        elif self.speed > self._required_speed:
            acceleration = self.configuration.decelleration
        else:
            acceleration = 0.0

        final_speed = self.speed + acceleration * deltatime
        if (acceleration > 0.0 and final_speed > self._required_speed) or (acceleration < 0.0 and final_speed < self._required_speed):
            # accelerate only for reaching the required speed
            acceleration = (self._required_speed - self.speed) / deltatime
            final_speed = self._required_speed

        movement = self.speed * deltatime + (0.5 * acceleration * (deltatime ** 2))
        dx = movement * cos(radians(self.direction))
        dy = movement * sin(radians(self.direction))
        self.pos_x += dx
        self.pos_y += dy

        self.speed = final_speed

        # COLLISION
        collision = self._board.detect_collision(self, (dx, dy))
        if collision is not None:
            self.block()
            self._pos_x, self.pos_y = collision[:2]
            self.take_damage(collision[2])

            # Manage BoardViewer Events
            e = EventRobotCollision()
            e.activation_time = self.simulation_time
            e.event_type = 6
            e.robot = self.get_robot_info()
            self._board.add_streamed_event(e)

    def take_damage(self, hp):
        """ Inflict a damage to the robot.
        :type hp: float
        :param hp:
        """
        self._health -= hp
        if self._health <= 0:
            self.block()
            self._health = 0
            self._is_dead = True

    def get_xy(self):
        """
        :rtype: (float, float)
        """
        return self._pos_x, self._pos_y
