package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import io.swagger.client.model.RobotInfo;
import java.math.BigDecimal;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class EventScan extends Event  {
  
  private BigDecimal eventType = null;
  private Float activationTime = null;
  private Float direction = null;
  private Float semiaperture = null;
  private Float scanMaxDistance = null;
  private RobotInfo robot = null;
  private RobotInfo hitRobot = null;

  
  /**
   * tag 3
   **/
  public EventScan eventType(BigDecimal eventType) {
    this.eventType = eventType;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "tag 3")
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
  public EventScan activationTime(Float activationTime) {
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
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public EventScan direction(Float direction) {
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
   * The aperture angle, in degree, divided by 2.
   * minimum: 0.0
   * maximum: 180.0
   **/
  public EventScan semiaperture(Float semiaperture) {
    this.semiaperture = semiaperture;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The aperture angle, in degree, divided by 2.")
  @JsonProperty("semiaperture")
  public Float getSemiaperture() {
    return semiaperture;
  }
  public void setSemiaperture(Float semiaperture) {
    this.semiaperture = semiaperture;
  }

  
  /**
   * The maximum possible distance of the scan.
   **/
  public EventScan scanMaxDistance(Float scanMaxDistance) {
    this.scanMaxDistance = scanMaxDistance;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "The maximum possible distance of the scan.")
  @JsonProperty("scanMaxDistance")
  public Float getScanMaxDistance() {
    return scanMaxDistance;
  }
  public void setScanMaxDistance(Float scanMaxDistance) {
    this.scanMaxDistance = scanMaxDistance;
  }

  
  /**
   **/
  public EventScan robot(RobotInfo robot) {
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
   **/
  public EventScan hitRobot(RobotInfo hitRobot) {
    this.hitRobot = hitRobot;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("hitRobot")
  public RobotInfo getHitRobot() {
    return hitRobot;
  }
  public void setHitRobot(RobotInfo hitRobot) {
    this.hitRobot = hitRobot;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EventScan eventScan = (EventScan) o;
    return Objects.equals(this.eventType, eventScan.eventType) &&
        Objects.equals(this.activationTime, eventScan.activationTime) &&
        Objects.equals(this.direction, eventScan.direction) &&
        Objects.equals(this.semiaperture, eventScan.semiaperture) &&
        Objects.equals(this.scanMaxDistance, eventScan.scanMaxDistance) &&
        Objects.equals(this.robot, eventScan.robot) &&
        Objects.equals(this.hitRobot, eventScan.hitRobot) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, activationTime, direction, semiaperture, scanMaxDistance, robot, hitRobot, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EventScan {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append("    activationTime: ").append(toIndentedString(activationTime)).append("\n");
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    semiaperture: ").append(toIndentedString(semiaperture)).append("\n");
    sb.append("    scanMaxDistance: ").append(toIndentedString(scanMaxDistance)).append("\n");
    sb.append("    robot: ").append(toIndentedString(robot)).append("\n");
    sb.append("    hitRobot: ").append(toIndentedString(hitRobot)).append("\n");
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

