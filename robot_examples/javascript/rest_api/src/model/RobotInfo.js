(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.RobotInfo = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The RobotInfo model module.
   * @module model/RobotInfo
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>RobotInfo</code>.
   * Info about the robot.
   * @alias module:model/RobotInfo
   * @class
   * @param robotId
   * @param posX
   * @param posY
   * @param direction
   * @param currentSpeed
   * @param requiredSpeed
   * @param acceleration
   * @param reloadingTime
   * @param health
   * @param points
   */
  var exports = function(robotId, posX, posY, direction, currentSpeed, requiredSpeed, acceleration, reloadingTime, health, points) {

    this['robotId'] = robotId;
    this['posX'] = posX;
    this['posY'] = posY;
    this['direction'] = direction;
    this['currentSpeed'] = currentSpeed;
    this['requiredSpeed'] = requiredSpeed;
    this['acceleration'] = acceleration;
    this['reloadingTime'] = reloadingTime;
    this['health'] = health;
    this['points'] = points;
  };

  /**
   * Constructs a <code>RobotInfo</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/RobotInfo} obj Optional instance to populate.
   * @return {module:model/RobotInfo} The populated <code>RobotInfo</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('robotId')) {
        obj['robotId'] = ApiClient.convertToType(data['robotId'], 'Number');
      }
      if (data.hasOwnProperty('posX')) {
        obj['posX'] = ApiClient.convertToType(data['posX'], 'Number');
      }
      if (data.hasOwnProperty('posY')) {
        obj['posY'] = ApiClient.convertToType(data['posY'], 'Number');
      }
      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('currentSpeed')) {
        obj['currentSpeed'] = ApiClient.convertToType(data['currentSpeed'], 'Number');
      }
      if (data.hasOwnProperty('requiredSpeed')) {
        obj['requiredSpeed'] = ApiClient.convertToType(data['requiredSpeed'], 'Number');
      }
      if (data.hasOwnProperty('acceleration')) {
        obj['acceleration'] = ApiClient.convertToType(data['acceleration'], 'Number');
      }
      if (data.hasOwnProperty('reloadingTime')) {
        obj['reloadingTime'] = ApiClient.convertToType(data['reloadingTime'], 'Number');
      }
      if (data.hasOwnProperty('health')) {
        obj['health'] = ApiClient.convertToType(data['health'], 'Number');
      }
      if (data.hasOwnProperty('points')) {
        obj['points'] = ApiClient.convertToType(data['points'], 'Number');
      }
    }
    return obj;
  }


  /**
   * @member {Number} robotId
   */
  exports.prototype['robotId'] = undefined;

  /**
   * @member {Number} posX
   */
  exports.prototype['posX'] = undefined;

  /**
   * @member {Number} posY
   */
  exports.prototype['posY'] = undefined;

  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * @member {Number} currentSpeed
   */
  exports.prototype['currentSpeed'] = undefined;

  /**
   * @member {Number} requiredSpeed
   */
  exports.prototype['requiredSpeed'] = undefined;

  /**
   * @member {Number} acceleration
   */
  exports.prototype['acceleration'] = undefined;

  /**
   * 0 if the robot can fire immediately.
   * @member {Number} reloadingTime
   */
  exports.prototype['reloadingTime'] = undefined;

  /**
   * 0 if the robot is dead.
   * @member {Number} health
   */
  exports.prototype['health'] = undefined;

  /**
   * the earned points.
   * @member {Number} points
   */
  exports.prototype['points'] = undefined;




  return exports;
}));
