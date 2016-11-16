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
Game Server.
"""

import tornado.ioloop
import tornado.web
from tornado import gen, escape
from tornado.concurrent import Future
import httplib
import urllib3
from rest_api.api_client import ApiClient
from server.rest_api.rest import RESTResponse
from server.rest_api.models.robot_configuration import RobotConfiguration
from server.rest_api.models.robot_command import RobotCommand
from server.rest_api.models.board_info import BoardInfo
from server.rest_api.models.robot_status import RobotStatus
from server.rest_api.models.drive_command import DriveCommand
from server.rest_api.models.fire_command import FireCommand
from server.game_model import Board, Robot

import os.path

import sys
import time
import traceback
import logging
from datetime import timedelta
from game_model import *
import json

# #############
# Routing rules

settings = {
    "static_path": os.path.join(os.path.dirname(__file__), "static"),
    "template_path": os.path.join(os.path.dirname(__file__), "templates")
}

def make_app():
    return tornado.web.Application([
        (r"/robot-create", CreateRobotHandler),
        (r"/robot-action", TurnHandler),
        (r"/", BoardViewerPageHandler),
        (r"/board-info", BoardInfoHandler),
        (r"/board-events", BoardEventsHandler),
        ], **settings)

def create_game_server(board_size_x, board_size_y, network_latency, simulation_delta_time):
    init_game_model(board_size_x, board_size_y, network_latency, simulation_delta_time)
    game_server = tornado.ioloop.PeriodicCallback(process_game_turn, network_latency * 1000.0)
    game_server.start()
    return game_server

def run_server(http_port, board_size_x, board_size_y, network_latency, simulation_delta_time):
    """
    :type http_port: int
    :type board_size_x: int
    :type board_size_y: int

    :type network_latency: float
    :type simulation_delta_time: float
    """
    create_game_server(board_size_x, board_size_y, network_latency, simulation_delta_time)

    app = make_app()
    app.listen(http_port)

    # Accepts the requests from the remote-clients
    tornado.ioloop.IOLoop.current().start()

# ###########
# Game Status

next_robot_commands = {}
""" :var next_robot_commands: dict[str, RobotCommand]

The key is the robot token.
The value is None in case the robot has not yet sent the command.
Only registered robots with a None command, can send commands.
NOTE: robots can send only one command for turn, so after there is a registered command,
all subsequent calls are prohibited.
"""

api_client = None
""" :var api_client: DefaultApi
Service used for generating the JSON answers.
"""

stream_delay = 2.0
""" :var stream_delay: Float
The time in seconds before sending a streamed EventList.
"""

simulation_time_at_which_send_streamed_board_events = 0.0

queued_events_handler_requests = []
""" :var queued_events_handler_requests: list[BoardEventsFuture]
"""

def register_robot(token):
    """Register a robot into next_robot_commands
    :type token: str
    """
    global next_robot_commands
    next_robot_commands[token] = None

def add_next_robot_command(command):
    """Follow the next_robot_commands API/behavior.
    :return: True if the command was added.
    False if the command is not valid: the robot does not exists, or the robot sent two or more commands in the same turn.
    :type command: RobotCommand
    :rtype: bool
    """
    global next_robot_commands

    token = command.token
    if token in next_robot_commands:
        if next_robot_commands[token] is None:
            next_robot_commands[token] = command
            return True
    return False

queued_turn_handler_requests = []
""" :var queued_turn_handler_requests: list[TurnAnswerFuture]
"""

game_board = None
""" :var game_board: Board
"""

def init_game_model(board_size_x, board_size_y, network_latency, simulation_delta_time):
    """
    :type board_size_x: int
    :type board_size_y: int

    :type network_latency: float
    :type simulation_delta_time: float
    """
    global api_client
    global game_board
    global simulation_time_at_which_send_streamed_board_events
    global stream_delay

    game_board = Board((board_size_x, board_size_y), simulation_delta_time, network_latency)
    api_client = ApiClient()
    simulation_time_at_which_send_streamed_board_events = game_board.get_simulation_time_inside_realtime(stream_delay)

# ######################
# Web Requests Handlers

class TurnAnswerFuture(Future):
    token = None

class BoardEventsFuture(Future):
    all_board_status = False

def process_game_turn():

    global queued_turn_handler_requests
    global next_robot_commands
    global game_board
    global stream_delay
    global simulation_time_at_which_send_streamed_board_events
    global queued_events_handler_requests

    # Send commands to robots

    new_next_robot_commands = {}
    from_old_to_new_tokens = {}
    for token, command in next_robot_commands.iteritems():
        """ :var command: RobotCommand
        """
        robot = game_board.get_robot_by_token(token)
        if command is not None:
            if robot is not None:
                robot.set_next_command(command)
                new_token = robot.token
                from_old_to_new_tokens[token] = new_token

                # A dead robot can not receive any more commands,
                # so does not register the new token.
                if not robot.is_dead:
                    new_next_robot_commands[new_token] = None
        else:
            new_next_robot_commands[token] = None
            # NOTE: it must create a new empty slot in any case

            if robot is not None:
                robot.missed_turns = robot.missed_turns + 1

    # Advance the board simulation.

    game_board.simulate_for_a_turn()

    # Return robot state to TurnHandlers

    handlers_to_process = queued_turn_handler_requests[:]
    # NOTE: create a new server state, because after we start answering to Handlers, they can start making request again

    queued_turn_handler_requests = []
    next_robot_commands = new_next_robot_commands

    for turn_handler in handlers_to_process:
        """ :var turn_handler: TurnAnswerFuture
        """
        old_token = turn_handler.token
        token = from_old_to_new_tokens[old_token]
        turn_handler.set_result(game_board.get_robot_by_token(token))

    # Process Board Viewer Streamed Events

    if game_board.global_time() >= simulation_time_at_which_send_streamed_board_events:

        handlers_to_process = queued_events_handler_requests[:]
        # NOTE: make a distinct copy, because from now there can be new requests
        queued_events_handler_requests = []

        # calculate the value to send to clients only one time (it is cached)
        board_info = BoardInfo()

        board_info.max_board_x = game_board.get_size_x()
        board_info.max_board_y = game_board.get_size_y()
        board_info.stream_delay = stream_delay
        board_info.network_latency = game_board.get_network_latency()
        board_info.turn_delta_time = game_board.get_turn_delta_time()
        board_info.start_time = game_board.get_streamed_events_start_time()
        board_info.end_time = game_board.global_time()
        board_info.events = game_board.get_streamed_events()
        incremental_events = escape.json_encode(api_client.sanitize_for_serialization(board_info))

        board_info.start_time = game_board.global_time()
        board_info.events = game_board.get_current_status()
        status_events = escape.json_encode(api_client.sanitize_for_serialization(board_info))

        # prepare for receiving new requests from clients, before sending answers
        simulation_time_at_which_send_streamed_board_events = game_board.global_time()  + game_board.get_simulation_time_inside_realtime(stream_delay)
        game_board.reset_streamed_events()

        for handler in handlers_to_process:
            if handler.all_board_status:
                handler.set_result(status_events)
            else:
                handler.set_result(incremental_events)

class TurnHandler(tornado.web.RequestHandler):
    """Receive commands requests from remote-clients.
    It is a coroutine because the answer is not sent immediately, but only at the end of the turn.

    A client robot send a command, and it will receive the answer only at the end of the turn.
    The Game Server suspend answers to clients, until a game-turn is reached.
    In this way the game is fair because every robot can issue only a command for turn.
    """

    @gen.coroutine
    def post(self):
        global queued_turn_handler_requests
        global api_client

        if self.request.headers["Content-Type"].startswith("application/json"):
            try:
                rr = RESTResponse(urllib3.response.HTTPResponse(self.request.body))
                command = api_client.deserialize(rr, RobotCommand)
                if command is None:
                    self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
                else:
                    if add_next_robot_command(command):
                        handler_future = TurnAnswerFuture()
                        handler_future.token = command.token
                        queued_turn_handler_requests.append(handler_future)
                        robot_status = yield handler_future
                        self.set_header("Content-Type", "application/json")
                        self.write(api_client.sanitize_for_serialization(robot_status))
                    else:
                        self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

            except:
                self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
        else:
            self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

class CreateRobotHandler(tornado.web.RequestHandler):
    """Create a new robot.
    """

    @gen.coroutine
    def post(self):
        global queued_turn_handler_requests
        global api_client
        global game_board

        if self.request.headers["Content-Type"].startswith("application/json"):
            try:
                rr = RESTResponse(urllib3.response.HTTPResponse(body=self.request.body))
                command = api_client.deserialize(rr, RobotConfiguration)
                if command is None:
                    self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
                else:
                    robot_status = game_board.create_robot(command)
                    register_robot(robot_status.token)
                    self.set_header("Content-Type", "application/json")
                    self.write(api_client.sanitize_for_serialization(robot_status))
            except BaseException as e:
                self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

        else:
            self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

class BoardViewerPageHandler(tornado.web.RequestHandler):
    def get(self):
        self.render("board.html")

class BoardInfoHandler(tornado.web.RequestHandler):
    @gen.coroutine
    def get(self):
        global queued_turn_handler_requests
        global api_client
        global game_board
        global stream_delay
        global queued_events_handler_requests

        try:
            handler_future = BoardEventsFuture()
            handler_future.all_board_status = True
            queued_events_handler_requests.append(handler_future)
            board_info = yield handler_future
            self.set_header("Content-Type", "application/json")
            self.write(board_info)

        except:
            self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

class BoardEventsHandler(tornado.web.RequestHandler):
    """Accumulate for stream_delay seconds the board viewer events,
    and then send the answer with all the events generated in the stream_delay time-frame to all clients.

    This method is more efficient respect direct answer because:
    - the answer is calculated only one time, and it is reused for all viewers
    - board update events are small but frequent events, so it is better sending few packets with a lots of data
    """

    @gen.coroutine
    def get(self):
        global api_client
        global game_board
        global stream_delay
        global simulation_time_at_which_send_streamed_board_events
        global queued_events_handler_requests

        try:
            handler_future = BoardEventsFuture()
            handler_future.all_board_status = False
            queued_events_handler_requests.append(handler_future)
            events = yield handler_future
            self.set_header("Content-Type", "application/json")
            self.write(events)
        except:
            self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

