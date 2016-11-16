package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import io.swagger.client.model.RobotInfo;
import java.math.BigDecimal;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class EventRemoveRobot extends Event  {
  
  private BigDecimal eventType = null;
  private Float activationTime = null;
  private RobotInfo robot = null;

  
  /**
   * tag 2
   **/
  public EventRemoveRobot eventType(BigDecimal eventType) {
    this.eventType = eventType;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "tag 2")
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
  public EventRemoveRobot activationTime(Float activationTime) {
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
  public EventRemoveRobot robot(RobotInfo robot) {
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

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EventRemoveRobot eventRemoveRobot = (EventRemoveRobot) o;
    return Objects.equals(this.eventType, eventRemoveRobot.eventType) &&
        Objects.equals(this.activationTime, eventRemoveRobot.activationTime) &&
        Objects.equals(this.robot, eventRemoveRobot.robot) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, activationTime, robot, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EventRemoveRobot {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append("    activationTime: ").append(toIndentedString(activationTime)).append("\n");
    sb.append("    robot: ").append(toIndentedString(robot)).append("\n");
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

