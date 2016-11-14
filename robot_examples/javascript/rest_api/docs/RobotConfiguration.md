# NetRobotsApi.RobotConfiguration

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **String** |  | 
**maxHitPoints** | **Number** | The initial health of the robot. Default value: 100.0 | [optional] 
**maxSpeed** | **Number** | Max speed of the robot. Default value: 27.0 m/s | [optional] 
**acceleration** | **Number** | Acceleration of the robot. Default value 9.0 m/s^2 | [optional] 
**decelleration** | **Number** | The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2 | [optional] 
**maxSterlingSpeed** | **Number** | The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n | [optional] 
**maxScanDistance** | **Number** | The max distance the robot can identify targets. Default value: 700 m/s. | [optional] 
**maxFireDistance** | **Number** | The max distance a bullet can reach. Default value: 700 m/s. | [optional] 
**bulletSpeed** | **Number** | The speed of a fired bullet. Default value: 500 m/s. | [optional] 
**bulletDamage** | **Number** | The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 50% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, there is an associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum possibile value.\n | [optional] 
**cannonReloadingTime** | **Number** | How many seconds the robot must wait before firing another missile. Min value: 2s. Max value: 6s. Deafault value: 2s.\n | [optional] 


