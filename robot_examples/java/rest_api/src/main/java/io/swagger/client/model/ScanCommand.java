package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;





@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class ScanCommand   {
  
  private Float direction = null;
  private Float semiaperture = null;

  
  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * minimum: 0.0
   * maximum: 359.0
   **/
  public ScanCommand direction(Float direction) {
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
  public ScanCommand semiaperture(Float semiaperture) {
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

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ScanCommand scanCommand = (ScanCommand) o;
    return Objects.equals(this.direction, scanCommand.direction) &&
        Objects.equals(this.semiaperture, scanCommand.semiaperture);
  }

  @Override
  public int hashCode() {
    return Objects.hash(direction, semiaperture);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ScanCommand {\n");
    
    sb.append("    direction: ").append(toIndentedString(direction)).append("\n");
    sb.append("    semiaperture: ").append(toIndentedString(semiaperture)).append("\n");
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

