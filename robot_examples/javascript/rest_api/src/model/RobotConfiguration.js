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
    root.NetRobotsApi.RobotConfiguration = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The RobotConfiguration model module.
   * @module model/RobotConfiguration
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>RobotConfiguration</code>.
   * Configurations of the robot.\n
   * @alias module:model/RobotConfiguration
   * @class
   * @param name
   */
  var exports = function(name) {

    this['name'] = name;










  };

  /**
   * Constructs a <code>RobotConfiguration</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/RobotConfiguration} obj Optional instance to populate.
   * @return {module:model/RobotConfiguration} The populated <code>RobotConfiguration</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('name')) {
        obj['name'] = ApiClient.convertToType(data['name'], 'String');
      }
      if (data.hasOwnProperty('maxHitPoints')) {
        obj['maxHitPoints'] = ApiClient.convertToType(data['maxHitPoints'], 'Number');
      }
      if (data.hasOwnProperty('maxSpeed')) {
        obj['maxSpeed'] = ApiClient.convertToType(data['maxSpeed'], 'Number');
      }
      if (data.hasOwnProperty('acceleration')) {
        obj['acceleration'] = ApiClient.convertToType(data['acceleration'], 'Number');
      }
      if (data.hasOwnProperty('decelleration')) {
        obj['decelleration'] = ApiClient.convertToType(data['decelleration'], 'Number');
      }
      if (data.hasOwnProperty('maxSterlingSpeed')) {
        obj['maxSterlingSpeed'] = ApiClient.convertToType(data['maxSterlingSpeed'], 'Number');
      }
      if (data.hasOwnProperty('maxScanDistance')) {
        obj['maxScanDistance'] = ApiClient.convertToType(data['maxScanDistance'], 'Number');
      }
      if (data.hasOwnProperty('maxFireDistance')) {
        obj['maxFireDistance'] = ApiClient.convertToType(data['maxFireDistance'], 'Number');
      }
      if (data.hasOwnProperty('bulletSpeed')) {
        obj['bulletSpeed'] = ApiClient.convertToType(data['bulletSpeed'], 'Number');
      }
      if (data.hasOwnProperty('bulletDamage')) {
        obj['bulletDamage'] = ApiClient.convertToType(data['bulletDamage'], 'Number');
      }
      if (data.hasOwnProperty('cannonReloadingTime')) {
        obj['cannonReloadingTime'] = ApiClient.convertToType(data['cannonReloadingTime'], 'Number');
      }
    }
    return obj;
  }


  /**
   * @member {String} name
   */
  exports.prototype['name'] = undefined;

  /**
   * The initial health of the robot. Default value: 100.0
   * @member {Number} maxHitPoints
   */
  exports.prototype['maxHitPoints'] = undefined;

  /**
   * Max speed of the robot. Default value: 27.0 m/s
   * @member {Number} maxSpeed
   */
  exports.prototype['maxSpeed'] = undefined;

  /**
   * Acceleration of the robot. Default value 9.0 m/s^2
   * @member {Number} acceleration
   */
  exports.prototype['acceleration'] = undefined;

  /**
   * The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2
   * @member {Number} decelleration
   */
  exports.prototype['decelleration'] = undefined;

  /**
   * The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n
   * @member {Number} maxSterlingSpeed
   */
  exports.prototype['maxSterlingSpeed'] = undefined;

  /**
   * The max distance the robot can identify targets. Default value: 700 m/s.
   * @member {Number} maxScanDistance
   */
  exports.prototype['maxScanDistance'] = undefined;

  /**
   * The max distance a bullet can reach. Default value: 700 m/s.
   * @member {Number} maxFireDistance
   */
  exports.prototype['maxFireDistance'] = undefined;

  /**
   * The speed of a fired bullet. Default value: 500 m/s.
   * @member {Number} bulletSpeed
   */
  exports.prototype['bulletSpeed'] = undefined;

  /**
   * The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 50% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, there is an associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum possibile value.\n
   * @member {Number} bulletDamage
   */
  exports.prototype['bulletDamage'] = undefined;

  /**
   * How many seconds the robot must wait before firing another missile. Min value: 2s. Max value: 6s. Deafault value: 2s.\n
   * @member {Number} cannonReloadingTime
   */
  exports.prototype['cannonReloadingTime'] = undefined;




  return exports;
}));
