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


from server.game_model import Robot
from server.game_model import Board
from server.rest_api.models.robot_status import RobotStatus
from server.rest_api.models.robot_configuration import RobotConfiguration
from server.rest_api.models.scan_status import ScanStatus
from server.rest_api.models.drive_command import DriveCommand
from server.rest_api.models.scan_command import ScanCommand
from server.rest_api.models.robot_command import RobotCommand

import unittest

class BoardTestCase(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def create_default_board(self):
        """
        :rtype: Board
        """
        return Board((1000, 1000), 1.0, 0.1)

    def create_default_robot_configuration(self, robotName):
        """
        :type robotName: string
        :rtype: RobotConfiguration
        """
        r = RobotConfiguration()

        r.name = robotName
        r.max_hit_points = 100.0
        r.max_speed = 27.0  # m/s
        r.acceleration = 9.0  # m/s^2
        r.decelleration = -5.0  # m/s^2
        r.max_sterling_speed = 13.0
        r.max_scan_distance = 700.0
        r.max_fire_distance = 700.0
        r.bullet_speed = 500.0  # m/s
        r.bullet_damage = 10.0
        r.cannon_reloading_time = 1.0
        # NOTE: strenght is optimized from the server

        return r

    def test_robot_init(self):
        board = self.create_default_board()

        robot1 = board.create_robot(self.create_default_robot_configuration('GUNDAM1'))
        self.assertEqual(robot1.name, 'GUNDAM1')
        self.assertEqual(robot1.is_dead, False)
        self.assertEqual(robot1.health, 100.0)
        self.assertEqual(robot1.simulation_time, 0.0)
        self.assertEqual(robot1.time_tick, 1.0)
        self.assertEqual(robot1.real_time_tick, 0.1)

        # create a Robot too much strong, that can not be initializated
        conf = self.create_default_robot_configuration("GUNDAM2")
        conf.max_speed = conf.max_speed * 10.0
        conf.max_hit_points = conf.max_hit_points * 10
        robot1 = board.create_robot(conf)
        self.assertEqual(robot1.name, 'GUNDAM2')
        self.assertEqual(robot1.is_dead, True)



    def test_robot_acceleration_1(self):
        board = self.create_default_board()
        robot = board.create_robot(self.create_default_robot_configuration('GUNDAM'))
        robot.pos_x = 250.0
        robot.pos_y = 500.0
        self.assertEqual(robot.simulation_time, 0.0)
        self.assertEqual(robot.time_tick, 1.0)
        self.assertEqual(robot.real_time_tick, 0.1)

        robot.drive(0.0, 30.0)
        self.assertEqual(robot.direction, 0.0)
        self.assertEqual(robot.speed, 0.0)

        robot.tick(3.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 290.5)
        self.assertAlmostEqual(robot.speed, 27.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertEqual(robot.simulation_time, 3.0)
        self.assertEqual(robot.time_tick, 1.0)
        self.assertEqual(robot.real_time_tick, 0.1)

        robot.tick(3.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 371.5)
        self.assertAlmostEqual(robot.speed, 27.0)
        self.assertAlmostEqual(robot.direction, 0.0)

    def test_acceleration_2(self):
        board = self.create_default_board()
        robot = board.create_robot(self.create_default_robot_configuration('GUNDAM'))
        robot.pos_x = 250.0
        robot.pos_y = 500.0

        robot.drive(0.0, 30.0)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        robot.tick(0.25)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.speed, 27.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.configuration.max_speed, 27.0)
        self.assertAlmostEqual(robot.configuration.acceleration, 9.0)
        self.assertAlmostEqual(robot.pos_x, 290.5)

        robot.tick(0.3333333333333333333333333333)
        robot.tick(0.3333333333333333333333333333)
        robot.tick(0.3333333333333333333333333333)
        robot.tick(1.0)
        robot.tick(1.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 371.5)
        self.assertAlmostEqual(robot.speed, 27.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.configuration.max_speed, 27.0)

    def test_decelleration(self):
        board = self.create_default_board()
        robot = board.create_robot(self.create_default_robot_configuration('GUNDAM'))
        robot.pos_x = 250.0
        robot.pos_y = 500.0

        robot.drive(0.0, 30.0)
        robot.tick(3.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 290.5)
        self.assertAlmostEqual(robot.speed, 27.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.configuration.max_speed, 27.0)

        robot.drive(0.0, 0.0)
        robot.tick(3.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 349.0, places=1)
        self.assertAlmostEqual(robot.speed, 12.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.configuration.max_speed, 27.0)

        robot.tick(2.4)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 363.4, places=1)
        self.assertAlmostEqual(robot.speed, 0.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.configuration.max_speed, 27.0)

    def test_cannon(self):
        board = self.create_default_board()
        robot = board.create_robot(self.create_default_robot_configuration('GUNDAM'))
        robot.pos_x = 250.0
        robot.pos_y = 500.0

        robot.fire(0.0, 500.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 250.0)
        self.assertAlmostEqual(robot.speed, 0.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.cannon_reloading_time, robot.configuration.cannon_reloading_time)
        self.assertEqual(robot.fired_new_missile, True)

        robot.tick(1.0)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 250.0)
        self.assertAlmostEqual(robot.speed, 0.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.cannon_reloading_time, robot.configuration.cannon_reloading_time - 1.0)
        self.assertEqual(robot.fired_new_missile, False)

        robot.tick(robot.configuration.cannon_reloading_time)
        self.assertEqual(robot.pos_y, 500.0)
        self.assertAlmostEqual(robot.pos_x, 250.0)
        self.assertAlmostEqual(robot.speed, 0.0)
        self.assertAlmostEqual(robot.direction, 0.0)
        self.assertAlmostEqual(robot.cannon_reloading_time, 0.0)
        self.assertEqual(robot.fired_new_missile, False)

    def test_scanner(self):
        board = self.create_default_board()
        robot1 = board.create_robot(self.create_default_robot_configuration('GUNDAM1'))
        robot2 = board.create_robot(self.create_default_robot_configuration('GUNDAM2'))

        robot1.pos_x = 250.0
        robot1.pos_y = 500.0

        robot2.pos_x = 300.0
        robot2.pos_y = 500.0

        robot1.scan(0.0, 10.0)
        self.assertAlmostEqual(robot1.scan_status.distance, 50.0)

        robot2.scan(180.0, 10.0)
        self.assertAlmostEqual(robot2.scan_status.distance, 50.0)

    def test_distance(self):
        board = self.create_default_board()
        robot = board.create_robot(self.create_default_robot_configuration('GUNDAM'))
        robot.pos_x = 250.0
        robot.pos_y = 500.0

        self.assertEqual(robot.distance_from_point((500.0, 500.0)), (250.0, 0.0))
        self.assertEqual(robot.distance_from_point((250.0, 1000.0)), (500.0, 90.0))

    def test_robot_collision(self):
        board = self.create_default_board()

        robot1 = board.create_robot(self.create_default_robot_configuration('GUNDAM1'))
        robot2 = board.create_robot(self.create_default_robot_configuration('GUNDAM2'))

        robot1.pos_x = 250.0
        robot1.pos_y = 500.0

        robot2.pos_x = 750.0
        robot2.pos_y = 500.0

        self.assertFalse(robot1.is_dead)
        self.assertFalse(robot2.is_dead)
        self.assertAlmostEqual(robot1.speed, 0.0)
        self.assertAlmostEqual(robot2.speed, 0.0)

        robot1.drive(0.0, 27.0)
        robot2.drive(180.0, 27.0)

        while robot1.health > 99.9:
            board.simulate_for_a_turn()

        self.assertAlmostEqual(robot1.health, 98.0)
        self.assertAlmostEqual(robot2.health, 98.0)
        self.assertAlmostEqual(robot1.pos_x, 500.0, delta=2)
        self.assertAlmostEqual(robot2.pos_x, 500.0, delta=2)

        distance, angle = robot1.distance(robot2)
        self.assertAlmostEqual(distance, 2.0)
        self.assertAlmostEqual(angle, 0.0)

        robot1.drive(180.0, 1.0)
        board.simulate_for_a_turn()

        self.assertAlmostEqual(robot1.distance(robot2)[0], 2.9444, places=2)
        self.assertAlmostEqual(robot1.distance(robot2)[1], 0.0)

    def test_wall_collision(self):
        board = self.create_default_board()

        robot1 = board.create_robot(self.create_default_robot_configuration('GUNDAM1'))
        robot1.pos_x = 250.0
        robot1.pos_y = 500.0

        robot1.drive(45.0, 27.0)
        
        while robot1.health == 100.0:
            board.simulate_for_a_turn()
            
        self.assertAlmostEqual(robot1.pos_x, 750.0)
        self.assertAlmostEqual(robot1.pos_y, 1000.0)

        robot1.drive(-45.0, 27.0)
        while robot1.health == 98.0:
            board.simulate_for_a_turn()
            
        self.assertAlmostEqual(robot1.pos_x, 1000.0)
        self.assertAlmostEqual(robot1.pos_y, 750.0)

        robot1.drive(180.0 + 45.0, 27.0)
        while robot1.health == 96.0:
            board.simulate_for_a_turn()
            
        self.assertAlmostEqual(robot1.pos_x, 250.0)
        self.assertAlmostEqual(robot1.pos_y, 0.0)

        robot1.drive(180 - 45, 27)
        while robot1.health == 94.0:
            board.simulate_for_a_turn()
        self.assertAlmostEqual(robot1.pos_x, 0.0)
        self.assertAlmostEqual(robot1.pos_y, 250.0)

    def test_cannon(self):
        board = self.create_default_board()

        robot1 = board.create_robot(self.create_default_robot_configuration('GUNDAM1'))
        robot2 = board.create_robot(self.create_default_robot_configuration('GUNDAM2'))

        robot1.pos_x = 250.0
        robot1.pos_y = 500.0
        robot1.configuration.bullet_damage = 10.0

        robot2.pos_x = 750.0
        robot2.pos_y = 500.0
        robot2.configuration.bullet_damage = 10.0

        self.assertAlmostEqual(robot1.distance(robot2)[0], 500.0)
        self.assertAlmostEqual(robot1.distance(robot2)[1], 0.0)

        robot2.health = 100.0
        robot1.fire(0.0, 500.0)  # full hit
        while len(board.get_missiles()):
            board.simulate_for_a_turn()
        self.assertAlmostEqual(robot1.configuration.bullet_damage, 10,0)
        self.assertAlmostEqual(robot2.health, 90.0)

        while robot1.cannon_reloading_time > 0.0:
            board.simulate_for_a_turn()

        robot2.health = 100.0
        robot1.fire(0.0, 497.0)  # almost-hit
        while len(board.get_missiles()):
            board.simulate_for_a_turn()
        self.assertAlmostEqual(robot1.configuration.bullet_damage, 10,0)
        self.assertAlmostEqual(robot2.health, 95.116, places=2)

        while robot1.cannon_reloading_time > 0.0:
            board.simulate_for_a_turn()

        robot2.health = 100.0
        robot1.fire(0.0, 470.0)  # barely-hit
        while len(board.get_missiles()):
            board.simulate_for_a_turn()
        self.assertAlmostEqual(robot1.configuration.bullet_damage, 10,0)
        self.assertAlmostEqual(robot2.health, 98.255, places=2)

if __name__ == '__main__':
    unittest.main()
