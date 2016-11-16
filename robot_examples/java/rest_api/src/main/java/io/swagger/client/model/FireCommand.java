package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



/**
 * Missiles have a balistic trajectory, so they do not intercept robots in the middle of the path, but they damage only robot at the end of the path. Missile outside arena make no arm.
 **/

@ApiModel(description = "Missiles have a balistic trajectory, so they do not intercept robots in the middle of the path, but they damage only robot at the end of the path. Missile outside arena make no arm.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class FireCommand   {
  
  private Float direction = null;
  private Float distance = null;

  
  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public FireCommand direction(Float direction) {
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
  public FireCommand distance(Float distance) {
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

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FireCommand fireCommand = (FireCommand) o;
    return Objects.equals(this.direction, fireCommand.direction) &&
        Objects.equals(this.distance, fireCommand.distance);
  }

  @Override
  public int hashCode() {
    return Objects.hash(direction, distance);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FireCommand {\n");
    
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    distance: ").append(toIndentedString(distance)).append("\n");
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

