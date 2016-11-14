package swagger

import (
)

type RobotInfo struct {
    RobotId  Number  `json:"robotId,omitempty"`
    PosX  float32  `json:"posX,omitempty"`
    PosY  float32  `json:"posY,omitempty"`
    Direction  float32  `json:"direction,omitempty"`
    CurrentSpeed  float32  `json:"currentSpeed,omitempty"`
    RequiredSpeed  float32  `json:"requiredSpeed,omitempty"`
    Acceleration  float32  `json:"acceleration,omitempty"`
    ReloadingTime  float32  `json:"reloadingTime,omitempty"`
    Health  float32  `json:"health,omitempty"`
    Points  float32  `json:"points,omitempty"`
    
}
