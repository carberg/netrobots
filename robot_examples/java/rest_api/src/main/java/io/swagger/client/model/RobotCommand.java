package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.DriveCommand;
import io.swagger.client.model.FireCommand;
import io.swagger.client.model.ScanCommand;



/**
 * A Robot can make 3 actions contemporary: drive, scan, fire. If an action can not be performed, it will be ignored. If the drive action is not specified, the robot will continue moving according the last drive command.
 **/

@ApiModel(description = "A Robot can make 3 actions contemporary: drive, scan, fire. If an action can not be performed, it will be ignored. If the drive action is not specified, the robot will continue moving according the last drive command.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-09T23:26:57.234+01:00")
public class RobotCommand   {
  
  private String token = null;
  private FireCommand fire = null;
  private DriveCommand drive = null;
  private ScanCommand scan = null;

  
  /**
   * A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
   **/
  public RobotCommand token(String token) {
    this.token = token;
    return this;
  }
  
  @ApiModelProperty(example = "null", required = true, value = "A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n")
  @JsonProperty("token")
  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }

  
  /**
   **/
  public RobotCommand fire(FireCommand fire) {
    this.fire = fire;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("fire")
  public FireCommand getFire() {
    return fire;
  }
  public void setFire(FireCommand fire) {
    this.fire = fire;
  }

  
  /**
   **/
  public RobotCommand drive(DriveCommand drive) {
    this.drive = drive;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("drive")
  public DriveCommand getDrive() {
    return drive;
  }
  public void setDrive(DriveCommand drive) {
    this.drive = drive;
  }

  
  /**
   **/
  public RobotCommand scan(ScanCommand scan) {
    this.scan = scan;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("scan")
  public ScanCommand getScan() {
    return scan;
  }
  public void setScan(ScanCommand scan) {
    this.scan = scan;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RobotCommand robotCommand = (RobotCommand) o;
    return Objects.equals(this.token, robotCommand.token) &&
        Objects.equals(this.fire, robotCommand.fire) &&
        Objects.equals(this.drive, robotCommand.drive) &&
        Objects.equals(this.scan, robotCommand.scan);
  }

  @Override
  public int hashCode() {
    return Objects.hash(token, fire, drive, scan);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RobotCommand {\n");
    
    sb.append("    token: ").append(toIndentedString(token)).append("\n");
    sb.append("    fire: ").append(toIndentedString(fire)).append("\n");
    sb.append("    drive: ").append(toIndentedString(drive)).append("\n");
    sb.append("    scan: ").append(toIndentedString(scan)).append("\n");
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

