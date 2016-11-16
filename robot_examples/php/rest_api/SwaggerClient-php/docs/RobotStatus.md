# RobotStatus

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **string** |  | 
**token** | **string** | A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n | 
**configuration** | [**\Swagger\Client\Model\RobotConfiguration**](RobotConfiguration.md) |  | 
**simulation_time** | **float** | The current simulation time, expressed in virtual simulated seconds. | 
**time_tick** | **float** | The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation. | 
**real_time_tick** | **float** | The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this is the real world time you have for sending the next command without loosing a turn.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n | 
**missed_turns** | [**Number**](Number.md) | 0 if this robot sent a command for each game turn. In case of network connection with high latency, the number of missed turns. | 
**points** | **float** | The sum of all hit points of the fired missiles. The robot with more hit points is the winner. | [optional] 
**health** | **float** | The health of a robot. 0 when a robot is dead (completely destroyed). | 
**is_dead** | **bool** | True if the robot is dead, or if during initial creation params are out of range. | 
**direction** | **float** | Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH | 
**speed** | **float** |  | 
**pos_x** | **float** |  | 
**pos_y** | **float** |  | 
**max_board_x** | **float** |  | 
**max_board_y** | **float** |  | 
**cannon_reloading_time** | **float** | 0 if the robot can fire immediately, the remaining time it must wait otherwise. | 
**fired_new_missile** | **bool** | True if the robot in last command fired a missile. | 
**scan_status** | [**\Swagger\Client\Model\ScanStatus**](ScanStatus.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


