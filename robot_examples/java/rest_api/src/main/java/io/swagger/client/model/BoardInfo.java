package io.swagger.client.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.client.model.Event;
import java.util.ArrayList;
import java.util.List;



/**
 * Initial settings of the board.
 **/

@ApiModel(description = "Initial settings of the board.")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-11-16T05:09:07.503+01:00")
public class BoardInfo   {
  
  private Float maxBoardX = null;
  private Float maxBoardY = null;
  private Float streamDelay = null;
  private Float turnDeltaTime = null;
  private Float networkLatency = null;
  private Float startTime = null;
  private Float endTime = null;
  private List<Event> events = new ArrayList<Event>();

  
  /**
   **/
  public BoardInfo maxBoardX(Float maxBoardX) {
    this.maxBoardX = maxBoardX;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("maxBoardX")
  public Float getMaxBoardX() {
    return maxBoardX;
  }
  public void setMaxBoardX(Float maxBoardX) {
    this.maxBoardX = maxBoardX;
  }

  
  /**
   **/
  public BoardInfo maxBoardY(Float maxBoardY) {
    this.maxBoardY = maxBoardY;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("maxBoardY")
  public Float getMaxBoardY() {
    return maxBoardY;
  }
  public void setMaxBoardY(Float maxBoardY) {
    this.maxBoardY = maxBoardY;
  }

  
  /**
   * the time in real seconds, beetwen event lists
   **/
  public BoardInfo streamDelay(Float streamDelay) {
    this.streamDelay = streamDelay;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "the time in real seconds, beetwen event lists")
  @JsonProperty("streamDelay")
  public Float getStreamDelay() {
    return streamDelay;
  }
  public void setStreamDelay(Float streamDelay) {
    this.streamDelay = streamDelay;
  }

  
  /**
   * Every robot can send and execute a command only after this simulated interval of time.
   **/
  public BoardInfo turnDeltaTime(Float turnDeltaTime) {
    this.turnDeltaTime = turnDeltaTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "Every robot can send and execute a command only after this simulated interval of time.")
  @JsonProperty("turnDeltaTime")
  public Float getTurnDeltaTime() {
    return turnDeltaTime;
  }
  public void setTurnDeltaTime(Float turnDeltaTime) {
    this.turnDeltaTime = turnDeltaTime;
  }

  
  /**
   * Every robot can send a command only after this interval of time.
   **/
  public BoardInfo networkLatency(Float networkLatency) {
    this.networkLatency = networkLatency;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "Every robot can send a command only after this interval of time.")
  @JsonProperty("networkLatency")
  public Float getNetworkLatency() {
    return networkLatency;
  }
  public void setNetworkLatency(Float networkLatency) {
    this.networkLatency = networkLatency;
  }

  
  /**
   * The events starts at the specified simulation time.
   **/
  public BoardInfo startTime(Float startTime) {
    this.startTime = startTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The events starts at the specified simulation time.")
  @JsonProperty("startTime")
  public Float getStartTime() {
    return startTime;
  }
  public void setStartTime(Float startTime) {
    this.startTime = startTime;
  }

  
  /**
   * The events ends at the specified simulation time.
   **/
  public BoardInfo endTime(Float endTime) {
    this.endTime = endTime;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "The events ends at the specified simulation time.")
  @JsonProperty("endTime")
  public Float getEndTime() {
    return endTime;
  }
  public void setEndTime(Float endTime) {
    this.endTime = endTime;
  }

  
  /**
   **/
  public BoardInfo events(List<Event> events) {
    this.events = events;
    return this;
  }
  
  @ApiModelProperty(example = "null", value = "")
  @JsonProperty("events")
  public List<Event> getEvents() {
    return events;
  }
  public void setEvents(List<Event> events) {
    this.events = events;
  }

  

  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    BoardInfo boardInfo = (BoardInfo) o;
    return Objects.equals(this.maxBoardX, boardInfo.maxBoardX) &&
        Objects.equals(this.maxBoardY, boardInfo.maxBoardY) &&
        Objects.equals(this.streamDelay, boardInfo.streamDelay) &&
        Objects.equals(this.turnDeltaTime, boardInfo.turnDeltaTime) &&
        Objects.equals(this.networkLatency, boardInfo.networkLatency) &&
        Objects.equals(this.startTime, boardInfo.startTime) &&
        Objects.equals(this.endTime, boardInfo.endTime) &&
        Objects.equals(this.events, boardInfo.events);
  }

  @Override
  public int hashCode() {
    return Objects.hash(maxBoardX, maxBoardY, streamDelay, turnDeltaTime, networkLatency, startTime, endTime, events);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BoardInfo {\n");
    
    sb.append("    maxBoardX: ").append(toIndentedString(maxBoardX)).append("\n");
    sb.append("    maxBoardY: ").append(toIndentedString(maxBoardY)).append("\n");
    sb.append("    streamDelay: ").append(toIndentedString(streamDelay)).append("\n");
    sb.append("    turnDeltaTime: ").append(toIndentedString(turnDeltaTime)).append("\n");
    sb.append("    networkLatency: ").append(toIndentedString(networkLatency)).append("\n");
    sb.append("    startTime: ").append(toIndentedString(startTime)).append("\n");
    sb.append("    endTime: ").append(toIndentedString(endTime)).append("\n");
    sb.append("    events: ").append(toIndentedString(events)).append("\n");
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

