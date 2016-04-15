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


class Event(object):
    """
    NOTE: This class is auto generated by the swagger code generator program.
    Do not edit the class manually.
    """
    def __init__(self):
        """
        Event - a model defined in Swagger

        :param dict swaggerTypes: The key is attribute name
                                  and the value is attribute type.
        :param dict attributeMap: The key is attribute name
                                  and the value is json key in definition.
        """
        self.swagger_types = {
            'event_type': 'float',
            'activation_time': 'float'
        }

        self.attribute_map = {
            'event_type': 'eventType',
            'activation_time': 'activationTime'
        }

        self._event_type = None
        self._activation_time = None

    @property
    def event_type(self):
        """
        Gets the event_type of this Event.
        The type of event generated. Used for simulating object-oriented subclassing.

        :return: The event_type of this Event.
        :rtype: float
        """
        return self._event_type

    @event_type.setter
    def event_type(self, event_type):
        """
        Sets the event_type of this Event.
        The type of event generated. Used for simulating object-oriented subclassing.

        :param event_type: The event_type of this Event.
        :type: float
        """
        self._event_type = event_type

    @property
    def activation_time(self):
        """
        Gets the activation_time of this Event.
        When the event become active.

        :return: The activation_time of this Event.
        :rtype: float
        """
        return self._activation_time

    @activation_time.setter
    def activation_time(self, activation_time):
        """
        Sets the activation_time of this Event.
        When the event become active.

        :param activation_time: The activation_time of this Event.
        :type: float
        """
        self._activation_time = activation_time

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

