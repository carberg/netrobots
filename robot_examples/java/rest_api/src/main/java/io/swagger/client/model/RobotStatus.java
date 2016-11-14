package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.RobotConfiguration;
import io.swagger.client.model.ScanStatus;



/**
 * The current status of the robot.
 **/

@ApiModel(description = "The current status of the robot.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-09T23:26:57.234+01:00")
public class RobotStatus   {
  
  private String name = null;
  private String token = null;
  private RobotConfiguration configuration = null;
  private Float simulationTime = null;
  private Float timeTick = null;
  private Float realTimeTick = null;
  private Float points = null;
  private Float health = null;
  private Boolean isDead = null;
  private Float direction = null;
  private Float speed = null;
  private Float posX = null;
  private Float posY = null;
  private Float maxBoardX = null;
  private Float maxBoardY = null;
  private Float cannonReloadingTime = null;
  private Boolean firedNewMissile = null;
  private ScanStatus scanStatus = null;

  
  /**
   **/
  public RobotStatus name(String name) {
    this.name = name;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  
  /**
   * A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
   **/
  public RobotStatus token(String token) {
    this.token = token;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n")
  @JsonProperty("token")
  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }

  
  /**
   **/
  public RobotStatus configuration(RobotConfiguration configuration) {
    this.configuration = configuration;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("configuration")
  public RobotConfiguration getConfiguration() {
    return configuration;
  }
  public void setConfiguration(RobotConfiguration configuration) {
    this.configuration = configuration;
  }

  
  /**
   * The current simulation time.
   **/
  public RobotStatus simulationTime(Float simulationTime) {
    this.simulationTime = simulationTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The current simulation time.")
  @JsonProperty("simulationTime")
  public Float getSimulationTime() {
    return simulationTime;
  }
  public void setSimulationTime(Float simulationTime) {
    this.simulationTime = simulationTime;
  }

  
  /**
   * The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.
   **/
  public RobotStatus timeTick(Float timeTick) {
    this.timeTick = timeTick;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.")
  @JsonProperty("timeTick")
  public Float getTimeTick() {
    return timeTick;
  }
  public void setTimeTick(Float timeTick) {
    this.timeTick = timeTick;
  }

  
  /**
   * The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this is the real world time you have for sending the next command without loosing a turn.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n
   **/
  public RobotStatus realTimeTick(Float realTimeTick) {
    this.realTimeTick = realTimeTick;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this is the real world time you have for sending the next command without loosing a turn.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n")
  @JsonProperty("realTimeTick")
  public Float getRealTimeTick() {
    return realTimeTick;
  }
  public void setRealTimeTick(Float realTimeTick) {
    this.realTimeTick = realTimeTick;
  }

  
  /**
   * The sum of all hit points of the fired missiles. The robot with more hit points is the winner.
   **/
  public RobotStatus points(Float points) {
    this.points = points;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The sum of all hit points of the fired missiles. The robot with more hit points is the winner.")
  @JsonProperty("points")
  public Float getPoints() {
    return points;
  }
  public void setPoints(Float points) {
    this.points = points;
  }

  
  /**
   * The health of a robot. 0 when a robot is dead (completely destroyed).
   **/
  public RobotStatus health(Float health) {
    this.health = health;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The health of a robot. 0 when a robot is dead (completely destroyed).")
  @JsonProperty("health")
  public Float getHealth() {
    return health;
  }
  public void setHealth(Float health) {
    this.health = health;
  }

  
  /**
   * True if the robot is dead, or if during initial creation params are out of range.
   **/
  public RobotStatus isDead(Boolean isDead) {
    this.isDead = isDead;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "True if the robot is dead, or if during initial creation params are out of range.")
  @JsonProperty("isDead")
  public Boolean getIsDead() {
    return isDead;
  }
  public void setIsDead(Boolean isDead) {
    this.isDead = isDead;
  }

  
  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public RobotStatus direction(Float direction) {
    this.direction = direction;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH")
  @JsonProperty("direction")
  public Float getDirection() {
    return direction;
  }
  public void setDirection(Float direction) {
    this.direction = direction;
  }

  
  /**
   **/
  public RobotStatus speed(Float speed) {
    this.speed = speed;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("speed")
  public Float getSpeed() {
    return speed;
  }
  public void setSpeed(Float speed) {
    this.speed = speed;
  }

  
  /**
   **/
  public RobotStatus posX(Float posX) {
    this.posX = posX;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("posX")
  public Float getPosX() {
    return posX;
  }
  public void setPosX(Float posX) {
    this.posX = posX;
  }

  
  /**
   **/
  public RobotStatus posY(Float posY) {
    this.posY = posY;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("posY")
  public Float getPosY() {
    return posY;
  }
  public void setPosY(Float posY) {
    this.posY = posY;
  }

  
  /**
   **/
  public RobotStatus maxBoardX(Float maxBoardX) {
    this.maxBoardX = maxBoardX;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("maxBoardX")
  public Float getMaxBoardX() {
    return maxBoardX;
  }
  public void setMaxBoardX(Float maxBoardX) {
    this.maxBoardX = maxBoardX;
  }

  
  /**
   **/
  public RobotStatus maxBoardY(Float maxBoardY) {
    this.maxBoardY = maxBoardY;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("maxBoardY")
  public Float getMaxBoardY() {
    return maxBoardY;
  }
  public void setMaxBoardY(Float maxBoardY) {
    this.maxBoardY = maxBoardY;
  }

  
  /**
   * 0 if the robot can fire immediately, the remaining time it must wait otherwise.
   **/
  public RobotStatus cannonReloadingTime(Float cannonReloadingTime) {
    this.cannonReloadingTime = cannonReloadingTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "0 if the robot can fire immediately, the remaining time it must wait otherwise.")
  @JsonProperty("cannonReloadingTime")
  public Float getCannonReloadingTime() {
    return cannonReloadingTime;
  }
  public void setCannonReloadingTime(Float cannonReloadingTime) {
    this.cannonReloadingTime = cannonReloadingTime;
  }

  
  /**
   * True if the robot in last command fired a missile.
   **/
  public RobotStatus firedNewMissile(Boolean firedNewMissile) {
    this.firedNewMissile = firedNewMissile;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "True if the robot in last command fired a missile.")
  @JsonProperty("firedNewMissile")
  public Boolean getFiredNewMissile() {
    return firedNewMissile;
  }
  public void setFiredNewMissile(Boolean firedNewMissile) {
    this.firedNewMissile = firedNewMissile;
  }

  
  /**
   **/
  public RobotStatus scanStatus(ScanStatus scanStatus) {
    this.scanStatus = scanStatus;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("scanStatus")
  public ScanStatus getScanStatus() {
    return scanStatus;
  }
  public void setScanStatus(ScanStatus scanStatus) {
    this.scanStatus = scanStatus;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RobotStatus robotStatus = (RobotStatus) o;
    return Objects.equals(this.name, robotStatus.name) &&
        Objects.equals(this.token, robotStatus.token) &&
        Objects.equals(this.configuration, robotStatus.configuration) &&
        Objects.equals(this.simulationTime, robotStatus.simulationTime) &&
        Objects.equals(this.timeTick, robotStatus.timeTick) &&
        Objects.equals(this.realTimeTick, robotStatus.realTimeTick) &&
        Objects.equals(this.points, robotStatus.points) &&
        Objects.equals(this.health, robotStatus.health) &&
        Objects.equals(this.isDead, robotStatus.isDead) &&
        Objects.equals(this.direction, robotStatus.direction) &&
        Objects.equals(this.speed, robotStatus.speed) &&
        Objects.equals(this.posX, robotStatus.posX) &&
        Objects.equals(this.posY, robotStatus.posY) &&
        Objects.equals(this.maxBoardX, robotStatus.maxBoardX) &&
        Objects.equals(this.maxBoardY, robotStatus.maxBoardY) &&
        Objects.equals(this.cannonReloadingTime, robotStatus.cannonReloadingTime) &&
        Objects.equals(this.firedNewMissile, robotStatus.firedNewMissile) &&
        Objects.equals(this.scanStatus, robotStatus.scanStatus);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, token, configuration, simulationTime, timeTick, realTimeTick, points, health, isDead, direction, speed, posX, posY, maxBoardX, maxBoardY, cannonReloadingTime, firedNewMissile, scanStatus);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RobotStatus {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    token: ").append(toIndentedString(token)).append("\n");
    sb.append("    configuration: ").append(toIndentedString(configuration)).append("\n");
    sb.append("    simulationTime: ").append(toIndentedString(simulationTime)).append("\n");
    sb.append("    timeTick: ").append(toIndentedString(timeTick)).append("\n");
    sb.append("    realTimeTick: ").append(toIndentedString(realTimeTick)).append("\n");
    sb.append("    points: ").append(toIndentedString(points)).append("\n");
    sb.append("    health: ").append(toIndentedString(health)).append("\n");
    sb.append("    isDead: ").append(toIndentedString(isDead)).append("\n");
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    speed: ").append(toIndentedString(speed)).append("\n");
    sb.append("    posX: ").append(toIndentedString(posX)).append("\n");
    sb.append("    posY: ").append(toIndentedString(posY)).append("\n");
    sb.append("    maxBoardX: ").append(toIndentedString(maxBoardX)).append("\n");
    sb.append("    maxBoardY: ").append(toIndentedString(maxBoardY)).append("\n");
    sb.append("    cannonReloadingTime: ").append(toIndentedString(cannonReloadingTime)).append("\n");
    sb.append("    firedNewMissile: ").append(toIndentedString(firedNewMissile)).append("\n");
    sb.append("    scanStatus: ").append(toIndentedString(scanStatus)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

