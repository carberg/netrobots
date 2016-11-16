package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import io.swagger.client.model.RobotInfo;
import java.math.BigDecimal;



/**
 * An event signaling to the UI, the creation of a Robot. eventType is 1.
 **/

@ApiModel(description = "An event signaling to the UI, the creation of a Robot. eventType is 1.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class EventCreateRobot extends Event  {
  
  private BigDecimal eventType = null;
  private Float activationTime = null;
  private RobotInfo robot = null;
  private String name = null;
  private String color = null;

  
  /**
   * tag 1
   **/
  public EventCreateRobot eventType(BigDecimal eventType) {
    this.eventType = eventType;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "tag 1")
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
  public EventCreateRobot activationTime(Float activationTime) {
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
  public EventCreateRobot robot(RobotInfo robot) {
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
   * Human readable name for the robot.
   **/
  public EventCreateRobot name(String name) {
    this.name = name;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "Human readable name for the robot.")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  
  /**
   * A color assigned to the robot.
   **/
  public EventCreateRobot color(String color) {
    this.color = color;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "A color assigned to the robot.")
  @JsonProperty("color")
  public String getColor() {
    return color;
  }
  public void setColor(String color) {
    this.color = color;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EventCreateRobot eventCreateRobot = (EventCreateRobot) o;
    return Objects.equals(this.eventType, eventCreateRobot.eventType) &&
        Objects.equals(this.activationTime, eventCreateRobot.activationTime) &&
        Objects.equals(this.robot, eventCreateRobot.robot) &&
        Objects.equals(this.name, eventCreateRobot.name) &&
        Objects.equals(this.color, eventCreateRobot.color) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, activationTime, robot, name, color, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EventCreateRobot {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append("    activationTime: ").append(toIndentedString(activationTime)).append("\n");
    sb.append("    robot: ").append(toIndentedString(robot)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
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

