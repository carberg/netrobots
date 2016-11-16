package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class DriveCommand   {
  
  private Float direction = null;
  private Float speed = null;

  
  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public DriveCommand direction(Float direction) {
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
  public DriveCommand speed(Float speed) {
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
    DriveCommand driveCommand = (DriveCommand) o;
    return Objects.equals(this.direction, driveCommand.direction) &&
        Objects.equals(this.speed, driveCommand.speed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(direction, speed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DriveCommand {\n");
    
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
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

