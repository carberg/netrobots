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
    root.NetRobotsApi.ScanStatus = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The ScanStatus model module.
   * @module model/ScanStatus
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>ScanStatus</code>.
   * The result of last scan command.
   * @alias module:model/ScanStatus
   * @class
   */
  var exports = function() {




  };

  /**
   * Constructs a <code>ScanStatus</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/ScanStatus} obj Optional instance to populate.
   * @return {module:model/ScanStatus} The populated <code>ScanStatus</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('semiApertureAngle')) {
        obj['semiApertureAngle'] = ApiClient.convertToType(data['semiApertureAngle'], 'Number');
      }
      if (data.hasOwnProperty('distance')) {
        obj['distance'] = ApiClient.convertToType(data['distance'], 'Number');
      }
    }
    return obj;
  }


  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * The aperture angle, in degree, divided by 2.
   * @member {Number} semiApertureAngle
   */
  exports.prototype['semiApertureAngle'] = undefined;

  /**
   * -1 if no robot was found.
   * @member {Number} distance
   */
  exports.prototype['distance'] = undefined;




  return exports;
}));
