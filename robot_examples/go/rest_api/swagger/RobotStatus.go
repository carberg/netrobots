package swagger

import (
)

type RobotStatus struct {
    Name  string  `json:"name,omitempty"`
    Token  string  `json:"token,omitempty"`
    Configuration  RobotConfiguration  `json:"configuration,omitempty"`
    SimulationTime  float32  `json:"simulationTime,omitempty"`
    TimeTick  float32  `json:"timeTick,omitempty"`
    RealTimeTick  float32  `json:"realTimeTick,omitempty"`
    MissedTurns  Number  `json:"missedTurns,omitempty"`
    Points  float32  `json:"points,omitempty"`
    Health  float32  `json:"health,omitempty"`
    IsDead  bool  `json:"isDead,omitempty"`
    Direction  float32  `json:"direction,omitempty"`
    Speed  float32  `json:"speed,omitempty"`
    PosX  float32  `json:"posX,omitempty"`
    PosY  float32  `json:"posY,omitempty"`
    MaxBoardX  float32  `json:"maxBoardX,omitempty"`
    MaxBoardY  float32  `json:"maxBoardY,omitempty"`
    CannonReloadingTime  float32  `json:"cannonReloadingTime,omitempty"`
    FiredNewMissile  bool  `json:"firedNewMissile,omitempty"`
    ScanStatus  ScanStatus  `json:"scanStatus,omitempty"`
    
}
