package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import io.swagger.client.model.RobotInfo;
import java.math.BigDecimal;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class EventMissile extends Event  {
  
  private BigDecimal eventType = null;
  private Float activationTime = null;
  private RobotInfo robot = null;
  private Float direction = null;
  private Float distance = null;
  private Float speed = null;

  
  /**
   * tag 4
   **/
  public EventMissile eventType(BigDecimal eventType) {
    this.eventType = eventType;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "tag 4")
  @JsonProperty("eventType")
  public BigDecimal getEventType() {
    return eventType;
  }
  public void setEventType(BigDecimal eventType) {
    this.eventType = eventType;
  }

  
  /**
   * When the event become active.
   **/
  public EventMissile activationTime(Float activationTime) {
    this.activationTime = activationTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "When the event become active.")
  @JsonProperty("activationTime")
  public Float getActivationTime() {
    return activationTime;
  }
  public void setActivationTime(Float activationTime) {
    this.activationTime = activationTime;
  }

  
  /**
   **/
  public EventMissile robot(RobotInfo robot) {
    this.robot = robot;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("robot")
  public RobotInfo getRobot() {
    return robot;
  }
  public void setRobot(RobotInfo robot) {
    this.robot = robot;
  }

  
  /**
   * minimum: 0.0
   * maximum: 359.0
   **/
  public EventMissile direction(Float direction) {
    this.direction = direction;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("direction")
  public Float getDirection() {
    return direction;
  }
  public void setDirection(Float direction) {
    this.direction = direction;
  }

  
  /**
   **/
  public EventMissile distance(Float distance) {
    this.distance = distance;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("distance")
  public Float getDistance() {
    return distance;
  }
  public void setDistance(Float distance) {
    this.distance = distance;
  }

  
  /**
   **/
  public EventMissile speed(Float speed) {
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

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EventMissile eventMissile = (EventMissile) o;
    return Objects.equals(this.eventType, eventMissile.eventType) &&
        Objects.equals(this.activationTime, eventMissile.activationTime) &&
        Objects.equals(this.robot, eventMissile.robot) &&
        Objects.equals(this.direction, eventMissile.direction) &&
        Objects.equals(this.distance, eventMissile.distance) &&
        Objects.equals(this.speed, eventMissile.speed) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, activationTime, robot, direction, distance, speed, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EventMissile {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append("    activationTime: ").append(toIndentedString(activationTime)).append("\n");
    sb.append("    robot: ").append(toIndentedString(robot)).append("\n");
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    distance: ").append(toIndentedString(distance)).append("\n");
    sb.append("    speed: ").append(toIndentedString(speed)).append("\n");
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

