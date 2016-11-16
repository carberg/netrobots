package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



/**
 * The result of last scan command.
 **/

@ApiModel(description = "The result of last scan command.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class ScanStatus   {
  
  private Float direction = null;
  private Float semiApertureAngle = null;
  private Float distance = null;

  
  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public ScanStatus direction(Float direction) {
    this.direction = direction;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH")
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
  public ScanStatus semiApertureAngle(Float semiApertureAngle) {
    this.semiApertureAngle = semiApertureAngle;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The aperture angle, in degree, divided by 2.")
  @JsonProperty("semiApertureAngle")
  public Float getSemiApertureAngle() {
    return semiApertureAngle;
  }
  public void setSemiApertureAngle(Float semiApertureAngle) {
    this.semiApertureAngle = semiApertureAngle;
  }

  
  /**
   * -1 if no robot was found.
   **/
  public ScanStatus distance(Float distance) {
    this.distance = distance;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "-1 if no robot was found.")
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
    ScanStatus scanStatus = (ScanStatus) o;
    return Objects.equals(this.direction, scanStatus.direction) &&
        Objects.equals(this.semiApertureAngle, scanStatus.semiApertureAngle) &&
        Objects.equals(this.distance, scanStatus.distance);
  }

  @Override
  public int hashCode() {
    return Objects.hash(direction, semiApertureAngle, distance);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ScanStatus {\n");
    
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    semiApertureAngle: ").append(toIndentedString(semiApertureAngle)).append("\n");
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

