package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import io.swagger.client.model.RobotInfo;
import java.math.BigDecimal;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class EventExplosion extends Event  {
  
  private BigDecimal eventType = null;
  private Float activationTime = null;
  private RobotInfo robot = null;
  private RobotInfo hitRobot = null;
  private Float damage = null;

  
  /**
   * tag 5
   **/
  public EventExplosion eventType(BigDecimal eventType) {
    this.eventType = eventType;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "tag 5")
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
  public EventExplosion activationTime(Float activationTime) {
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
  public EventExplosion robot(RobotInfo robot) {
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
  public EventExplosion hitRobot(RobotInfo hitRobot) {
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

  
  /**
   **/
  public EventExplosion damage(Float damage) {
    this.damage = damage;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "")
  @JsonProperty("damage")
  public Float getDamage() {
    return damage;
  }
  public void setDamage(Float damage) {
    this.damage = damage;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EventExplosion eventExplosion = (EventExplosion) o;
    return Objects.equals(this.eventType, eventExplosion.eventType) &&
        Objects.equals(this.activationTime, eventExplosion.activationTime) &&
        Objects.equals(this.robot, eventExplosion.robot) &&
        Objects.equals(this.hitRobot, eventExplosion.hitRobot) &&
        Objects.equals(this.damage, eventExplosion.damage) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, activationTime, robot, hitRobot, damage, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EventExplosion {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append("    activationTime: ").append(toIndentedString(activationTime)).append("\n");
    sb.append("    robot: ").append(toIndentedString(robot)).append("\n");
    sb.append("    hitRobot: ").append(toIndentedString(hitRobot)).append("\n");
    sb.append("    damage: ").append(toIndentedString(damage)).append("\n");
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

