# SwaggerClient::RobotInfo

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**robot_id** | **Float** |  | 
**pos_x** | **Float** | 0 is the left most point of the board. | 
**pos_y** | **Float** | 0 is the upper most point of the board. | 
**direction** | **Float** | Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH | 
**current_speed** | **Float** |  | 
**required_speed** | **Float** |  | 
**acceleration** | **Float** |  | 
**reloading_time** | **Float** | 0 if the robot can fire immediately. | 
**health** | **Float** | 0 if the robot is dead. | 
**points** | **Float** | the earned points. | 
**missed_turns** | **Float** | 0 if the robot sent a command for every game turn. | 


