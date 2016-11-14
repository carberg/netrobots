package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;



/**
 * Info about the robot.
 **/

@ApiModel(description = "Info about the robot.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-09T23:26:57.234+01:00")
public class RobotInfo   {
  
  private BigDecimal robotId = null;
  private Float posX = null;
  private Float posY = null;
  private Float direction = null;
  private Float currentSpeed = null;
  private Float requiredSpeed = null;
  private Float acceleration = null;
  private Float reloadingTime = null;
  private Float health = null;
  private Float points = null;

  
  /**
   **/
  public RobotInfo robotId(BigDecimal robotId) {
    this.robotId = robotId;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("robotId")
  public BigDecimal getRobotId() {
    return robotId;
  }
  public void setRobotId(BigDecimal robotId) {
    this.robotId = robotId;
  }

  
  /**
   **/
  public RobotInfo posX(Float posX) {
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
  public RobotInfo posY(Float posY) {
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
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public RobotInfo direction(Float direction) {
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
  public RobotInfo currentSpeed(Float currentSpeed) {
    this.currentSpeed = currentSpeed;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("currentSpeed")
  public Float getCurrentSpeed() {
    return currentSpeed;
  }
  public void setCurrentSpeed(Float currentSpeed) {
    this.currentSpeed = currentSpeed;
  }

  
  /**
   **/
  public RobotInfo requiredSpeed(Float requiredSpeed) {
    this.requiredSpeed = requiredSpeed;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("requiredSpeed")
  public Float getRequiredSpeed() {
    return requiredSpeed;
  }
  public void setRequiredSpeed(Float requiredSpeed) {
    this.requiredSpeed = requiredSpeed;
  }

  
  /**
   **/
  public RobotInfo acceleration(Float acceleration) {
    this.acceleration = acceleration;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("acceleration")
  public Float getAcceleration() {
    return acceleration;
  }
  public void setAcceleration(Float acceleration) {
    this.acceleration = acceleration;
  }

  
  /**
   * 0 if the robot can fire immediately.
   **/
  public RobotInfo reloadingTime(Float reloadingTime) {
    this.reloadingTime = reloadingTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "0 if the robot can fire immediately.")
  @JsonProperty("reloadingTime")
  public Float getReloadingTime() {
    return reloadingTime;
  }
  public void setReloadingTime(Float reloadingTime) {
    this.reloadingTime = reloadingTime;
  }

  
  /**
   * 0 if the robot is dead.
   **/
  public RobotInfo health(Float health) {
    this.health = health;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "0 if the robot is dead.")
  @JsonProperty("health")
  public Float getHealth() {
    return health;
  }
  public void setHealth(Float health) {
    this.health = health;
  }

  
  /**
   * the earned points.
   **/
  public RobotInfo points(Float points) {
    this.points = points;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "the earned points.")
  @JsonProperty("points")
  public Float getPoints() {
    return points;
  }
  public void setPoints(Float points) {
    this.points = points;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RobotInfo robotInfo = (RobotInfo) o;
    return Objects.equals(this.robotId, robotInfo.robotId) &&
        Objects.equals(this.posX, robotInfo.posX) &&
        Objects.equals(this.posY, robotInfo.posY) &&
        Objects.equals(this.direction, robotInfo.direction) &&
        Objects.equals(this.currentSpeed, robotInfo.currentSpeed) &&
        Objects.equals(this.requiredSpeed, robotInfo.requiredSpeed) &&
        Objects.equals(this.acceleration, robotInfo.acceleration) &&
        Objects.equals(this.reloadingTime, robotInfo.reloadingTime) &&
        Objects.equals(this.health, robotInfo.health) &&
        Objects.equals(this.points, robotInfo.points);
  }

  @Override
  public int hashCode() {
    return Objects.hash(robotId, posX, posY, direction, currentSpeed, requiredSpeed, acceleration, reloadingTime, health, points);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RobotInfo {\n");
    
    sb.append("    robotId: ").append(toIndentedString(robotId)).append("\n");
    sb.append("    posX: ").append(toIndentedString(posX)).append("\n");
    sb.append("    posY: ").append(toIndentedString(posY)).append("\n");
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    currentSpeed: ").append(toIndentedString(currentSpeed)).append("\n");
    sb.append("    requiredSpeed: ").append(toIndentedString(requiredSpeed)).append("\n");
    sb.append("    acceleration: ").append(toIndentedString(acceleration)).append("\n");
    sb.append("    reloadingTime: ").append(toIndentedString(reloadingTime)).append("\n");
    sb.append("    health: ").append(toIndentedString(health)).append("\n");
    sb.append("    points: ").append(toIndentedString(points)).append("\n");
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

