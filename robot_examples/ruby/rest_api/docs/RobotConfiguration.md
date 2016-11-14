# SwaggerClient::RobotConfiguration

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **String** |  | 
**max_hit_points** | **Float** | The initial health of the robot. Default value: 100.0 | [optional] 
**max_speed** | **Float** | Max speed of the robot. Default value: 27.0 m/s | [optional] 
**acceleration** | **Float** | Acceleration of the robot. Default value 9.0 m/s^2 | [optional] 
**decelleration** | **Float** | The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2 | [optional] 
**max_sterling_speed** | **Float** | The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n | [optional] 
**max_scan_distance** | **Float** | The max distance the robot can identify targets. Default value: 700 m/s. | [optional] 
**max_fire_distance** | **Float** | The max distance a bullet can reach. Default value: 700 m/s. | [optional] 
**bullet_speed** | **Float** | The speed of a fired bullet. Default value: 500 m/s. | [optional] 
**bullet_damage** | **Float** | The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 50% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, there is an associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum possibile value.\n | [optional] 
**cannon_reloading_time** | **Float** | How many seconds the robot must wait before firing another missile. Min value: 2s. Max value: 6s. Deafault value: 2s.\n | [optional] 


