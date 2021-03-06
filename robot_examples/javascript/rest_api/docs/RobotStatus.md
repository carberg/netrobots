# NetRobotsApi.RobotStatus

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **String** |  | 
**token** | **String** | A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n | 
**configuration** | [**RobotConfiguration**](RobotConfiguration.md) |  | 
**simulationTime** | **Number** | The current simulation time, expressed in virtual simulated seconds. | 
**timeTick** | **Number** | The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation. | 
**realTimeTick** | **Number** | The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this is the real world time you have for sending the next command without loosing a turn.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n | 
**missedTurns** | **Number** | 0 if this robot sent a command for each game turn. In case of network connection with high latency, the number of missed turns. | 
**points** | **Number** | The sum of all hit points of the fired missiles. The robot with more hit points is the winner. | [optional] 
**health** | **Number** | The health of a robot. 0 when a robot is dead (completely destroyed). | 
**isDead** | **Boolean** | True if the robot is dead, or if during initial creation params are out of range. | 
**direction** | **Number** | Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH | 
**speed** | **Number** |  | 
**posX** | **Number** |  | 
**posY** | **Number** |  | 
**maxBoardX** | **Number** |  | 
**maxBoardY** | **Number** |  | 
**cannonReloadingTime** | **Number** | 0 if the robot can fire immediately, the remaining time it must wait otherwise. | 
**firedNewMissile** | **Boolean** | True if the robot in last command fired a missile. | 
**scanStatus** | [**ScanStatus**](ScanStatus.md) |  | [optional] 


