# coding: utf-8

"""
Copyright 2016 SmartBear Software

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    Ref: https://github.com/swagger-api/swagger-codegen
"""

from pprint import pformat
from six import iteritems


class EventCreateRobot(object):
    """
    NOTE: This class is auto generated by the swagger code generator program.
    Do not edit the class manually.
    """
    def __init__(self):
        """
        EventCreateRobot - a model defined in Swagger

        :param dict swaggerTypes: The key is attribute name
                                  and the value is attribute type.
        :param dict attributeMap: The key is attribute name
                                  and the value is json key in definition.
        """
        self.swagger_types = {
            'event_type': 'float',
            'activation_time': 'float',
            'robot': 'RobotInfo',
            'name': 'str',
            'color': 'str'
        }

        self.attribute_map = {
            'event_type': 'eventType',
            'activation_time': 'activationTime',
            'robot': 'robot',
            'name': 'name',
            'color': 'color'
        }

        self._event_type = None
        self._activation_time = None
        self._robot = None
        self._name = None
        self._color = None

    @property
    def event_type(self):
        """
        Gets the event_type of this EventCreateRobot.
        tag 1

        :return: The event_type of this EventCreateRobot.
        :rtype: float
        """
        return self._event_type

    @event_type.setter
    def event_type(self, event_type):
        """
        Sets the event_type of this EventCreateRobot.
        tag 1

        :param event_type: The event_type of this EventCreateRobot.
        :type: float
        """
        self._event_type = event_type

    @property
    def activation_time(self):
        """
        Gets the activation_time of this EventCreateRobot.
        When the event become active.

        :return: The activation_time of this EventCreateRobot.
        :rtype: float
        """
        return self._activation_time

    @activation_time.setter
    def activation_time(self, activation_time):
        """
        Sets the activation_time of this EventCreateRobot.
        When the event become active.

        :param activation_time: The activation_time of this EventCreateRobot.
        :type: float
        """
        self._activation_time = activation_time

    @property
    def robot(self):
        """
        Gets the robot of this EventCreateRobot.


        :return: The robot of this EventCreateRobot.
        :rtype: RobotInfo
        """
        return self._robot

    @robot.setter
    def robot(self, robot):
        """
        Sets the robot of this EventCreateRobot.


        :param robot: The robot of this EventCreateRobot.
        :type: RobotInfo
        """
        self._robot = robot

    @property
    def name(self):
        """
        Gets the name of this EventCreateRobot.
        Human readable name for the robot.

        :return: The name of this EventCreateRobot.
        :rtype: str
        """
        return self._name

    @name.setter
    def name(self, name):
        """
        Sets the name of this EventCreateRobot.
        Human readable name for the robot.

        :param name: The name of this EventCreateRobot.
        :type: str
        """
        self._name = name

    @property
    def color(self):
        """
        Gets the color of this EventCreateRobot.
        A color assigned to the robot.

        :return: The color of this EventCreateRobot.
        :rtype: str
        """
        return self._color

    @color.setter
    def color(self, color):
        """
        Sets the color of this EventCreateRobot.
        A color assigned to the robot.

        :param color: The color of this EventCreateRobot.
        :type: str
        """
        self._color = color

    def to_dict(self):
        """
        Returns the model properties as a dict
        """
        result = {}

        for attr, _ in iteritems(self.swagger_types):
            value = getattr(self, attr)
            if isinstance(value, list):
                result[attr] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[attr] = value.to_dict()
            elif isinstance(value, dict):
                result[attr] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[attr] = value

        return result

    def to_str(self):
        """
        Returns the string representation of the model
        """
        return pformat(self.to_dict())

    def __repr__(self):
        """
        For `print` and `pprint`
        """
        return self.to_str()

    def __eq__(self, other):
        """
        Returns true if both objects are equal
        """
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """
        Returns true if both objects are not equal
        """
        return not self == other

