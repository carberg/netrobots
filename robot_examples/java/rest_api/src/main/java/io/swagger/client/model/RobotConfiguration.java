package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



/**
 * Configurations of the robot.\n
 **/

@ApiModel(description = "Configurations of the robot.\n")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-09T23:26:57.234+01:00")
public class RobotConfiguration   {
  
  private String name = null;
  private Float maxHitPoints = null;
  private Float maxSpeed = null;
  private Float acceleration = null;
  private Float decelleration = null;
  private Float maxSterlingSpeed = null;
  private Float maxScanDistance = null;
  private Float maxFireDistance = null;
  private Float bulletSpeed = null;
  private Float bulletDamage = null;
  private Float cannonReloadingTime = null;

  
  /**
   **/
  public RobotConfiguration name(String name) {
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
   * The initial health of the robot. Default value: 100.0
   **/
  public RobotConfiguration maxHitPoints(Float maxHitPoints) {
    this.maxHitPoints = maxHitPoints;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The initial health of the robot. Default value: 100.0")
  @JsonProperty("maxHitPoints")
  public Float getMaxHitPoints() {
    return maxHitPoints;
  }
  public void setMaxHitPoints(Float maxHitPoints) {
    this.maxHitPoints = maxHitPoints;
  }

  
  /**
   * Max speed of the robot. Default value: 27.0 m/s
   **/
  public RobotConfiguration maxSpeed(Float maxSpeed) {
    this.maxSpeed = maxSpeed;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "Max speed of the robot. Default value: 27.0 m/s")
  @JsonProperty("maxSpeed")
  public Float getMaxSpeed() {
    return maxSpeed;
  }
  public void setMaxSpeed(Float maxSpeed) {
    this.maxSpeed = maxSpeed;
  }

  
  /**
   * Acceleration of the robot. Default value 9.0 m/s^2
   **/
  public RobotConfiguration acceleration(Float acceleration) {
    this.acceleration = acceleration;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "Acceleration of the robot. Default value 9.0 m/s^2")
  @JsonProperty("acceleration")
  public Float getAcceleration() {
    return acceleration;
  }
  public void setAcceleration(Float acceleration) {
    this.acceleration = acceleration;
  }

  
  /**
   * The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2
   **/
  public RobotConfiguration decelleration(Float decelleration) {
    this.decelleration = decelleration;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2")
  @JsonProperty("decelleration")
  public Float getDecelleration() {
    return decelleration;
  }
  public void setDecelleration(Float decelleration) {
    this.decelleration = decelleration;
  }

  
  /**
   * The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n
   **/
  public RobotConfiguration maxSterlingSpeed(Float maxSterlingSpeed) {
    this.maxSterlingSpeed = maxSterlingSpeed;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n")
  @JsonProperty("maxSterlingSpeed")
  public Float getMaxSterlingSpeed() {
    return maxSterlingSpeed;
  }
  public void setMaxSterlingSpeed(Float maxSterlingSpeed) {
    this.maxSterlingSpeed = maxSterlingSpeed;
  }

  
  /**
   * The max distance the robot can identify targets. Default value: 700 m/s.
   **/
  public RobotConfiguration maxScanDistance(Float maxScanDistance) {
    this.maxScanDistance = maxScanDistance;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The max distance the robot can identify targets. Default value: 700 m/s.")
  @JsonProperty("maxScanDistance")
  public Float getMaxScanDistance() {
    return maxScanDistance;
  }
  public void setMaxScanDistance(Float maxScanDistance) {
    this.maxScanDistance = maxScanDistance;
  }

  
  /**
   * The max distance a bullet can reach. Default value: 700 m/s.
   **/
  public RobotConfiguration maxFireDistance(Float maxFireDistance) {
    this.maxFireDistance = maxFireDistance;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The max distance a bullet can reach. Default value: 700 m/s.")
  @JsonProperty("maxFireDistance")
  public Float getMaxFireDistance() {
    return maxFireDistance;
  }
  public void setMaxFireDistance(Float maxFireDistance) {
    this.maxFireDistance = maxFireDistance;
  }

  
  /**
   * The speed of a fired bullet. Default value: 500 m/s.
   **/
  public RobotConfiguration bulletSpeed(Float bulletSpeed) {
    this.bulletSpeed = bulletSpeed;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The speed of a fired bullet. Default value: 500 m/s.")
  @JsonProperty("bulletSpeed")
  public Float getBulletSpeed() {
    return bulletSpeed;
  }
  public void setBulletSpeed(Float bulletSpeed) {
    this.bulletSpeed = bulletSpeed;
  }

  
  /**
   * The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 50% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, there is an associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum possibile value.\n
   **/
  public RobotConfiguration bulletDamage(Float bulletDamage) {
    this.bulletDamage = bulletDamage;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 50% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, there is an associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum possibile value.\n")
  @JsonProperty("bulletDamage")
  public Float getBulletDamage() {
    return bulletDamage;
  }
  public void setBulletDamage(Float bulletDamage) {
    this.bulletDamage = bulletDamage;
  }

  
  /**
   * How many seconds the robot must wait before firing another missile. Min value: 2s. Max value: 6s. Deafault value: 2s.\n
   **/
  public RobotConfiguration cannonReloadingTime(Float cannonReloadingTime) {
    this.cannonReloadingTime = cannonReloadingTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "How many seconds the robot must wait before firing another missile. Min value: 2s. Max value: 6s. Deafault value: 2s.\n")
  @JsonProperty("cannonReloadingTime")
  public Float getCannonReloadingTime() {
    return cannonReloadingTime;
  }
  public void setCannonReloadingTime(Float cannonReloadingTime) {
    this.cannonReloadingTime = cannonReloadingTime;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RobotConfiguration robotConfiguration = (RobotConfiguration) o;
    return Objects.equals(this.name, robotConfiguration.name) &&
        Objects.equals(this.maxHitPoints, robotConfiguration.maxHitPoints) &&
        Objects.equals(this.maxSpeed, robotConfiguration.maxSpeed) &&
        Objects.equals(this.acceleration, robotConfiguration.acceleration) &&
        Objects.equals(this.decelleration, robotConfiguration.decelleration) &&
        Objects.equals(this.maxSterlingSpeed, robotConfiguration.maxSterlingSpeed) &&
        Objects.equals(this.maxScanDistance, robotConfiguration.maxScanDistance) &&
        Objects.equals(this.maxFireDistance, robotConfiguration.maxFireDistance) &&
        Objects.equals(this.bulletSpeed, robotConfiguration.bulletSpeed) &&
        Objects.equals(this.bulletDamage, robotConfiguration.bulletDamage) &&
        Objects.equals(this.cannonReloadingTime, robotConfiguration.cannonReloadingTime);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, maxHitPoints, maxSpeed, acceleration, decelleration, maxSterlingSpeed, maxScanDistance, maxFireDistance, bulletSpeed, bulletDamage, cannonReloadingTime);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RobotConfiguration {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    maxHitPoints: ").append(toIndentedString(maxHitPoints)).append("\n");
    sb.append("    maxSpeed: ").append(toIndentedString(maxSpeed)).append("\n");
    sb.append("    acceleration: ").append(toIndentedString(acceleration)).append("\n");
    sb.append("    decelleration: ").append(toIndentedString(decelleration)).append("\n");
    sb.append("    maxSterlingSpeed: ").append(toIndentedString(maxSterlingSpeed)).append("\n");
    sb.append("    maxScanDistance: ").append(toIndentedString(maxScanDistance)).append("\n");
    sb.append("    maxFireDistance: ").append(toIndentedString(maxFireDistance)).append("\n");
    sb.append("    bulletSpeed: ").append(toIndentedString(bulletSpeed)).append("\n");
    sb.append("    bulletDamage: ").append(toIndentedString(bulletDamage)).append("\n");
    sb.append("    cannonReloadingTime: ").append(toIndentedString(cannonReloadingTime)).append("\n");
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

