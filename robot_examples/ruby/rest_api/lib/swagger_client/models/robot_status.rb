=begin
NetRobots API

NetRobots API

OpenAPI spec version: 2.0.0

Generated by: https://github.com/swagger-api/swagger-codegen.git


=end

require 'date'

module SwaggerClient
  # The current status of the robot.
  class RobotStatus
    attr_accessor :name

    # A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
    attr_accessor :token

    attr_accessor :configuration

    # The current simulation time.
    attr_accessor :simulation_time

    # The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.
    attr_accessor :time_tick

    # The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this is the real world time you have for sending the next command without loosing a turn.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n
    attr_accessor :real_time_tick

    # The sum of all hit points of the fired missiles. The robot with more hit points is the winner.
    attr_accessor :points

    # The health of a robot. 0 when a robot is dead (completely destroyed).
    attr_accessor :health

    # True if the robot is dead, or if during initial creation params are out of range.
    attr_accessor :is_dead

    # Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
    attr_accessor :direction

    attr_accessor :speed

    attr_accessor :pos_x

    attr_accessor :pos_y

    attr_accessor :max_board_x

    attr_accessor :max_board_y

    # 0 if the robot can fire immediately, the remaining time it must wait otherwise.
    attr_accessor :cannon_reloading_time

    # True if the robot in last command fired a missile.
    attr_accessor :fired_new_missile

    attr_accessor :scan_status

    # Attribute mapping from ruby-style variable name to JSON key.
    def self.attribute_map
      {
        
        :'name' => :'name',
        
        :'token' => :'token',
        
        :'configuration' => :'configuration',
        
        :'simulation_time' => :'simulationTime',
        
        :'time_tick' => :'timeTick',
        
        :'real_time_tick' => :'realTimeTick',
        
        :'points' => :'points',
        
        :'health' => :'health',
        
        :'is_dead' => :'isDead',
        
        :'direction' => :'direction',
        
        :'speed' => :'speed',
        
        :'pos_x' => :'posX',
        
        :'pos_y' => :'posY',
        
        :'max_board_x' => :'maxBoardX',
        
        :'max_board_y' => :'maxBoardY',
        
        :'cannon_reloading_time' => :'cannonReloadingTime',
        
        :'fired_new_missile' => :'firedNewMissile',
        
        :'scan_status' => :'scanStatus'
        
      }
    end

    # Attribute type mapping.
    def self.swagger_types
      {
        :'name' => :'String',
        :'token' => :'String',
        :'configuration' => :'RobotConfiguration',
        :'simulation_time' => :'Float',
        :'time_tick' => :'Float',
        :'real_time_tick' => :'Float',
        :'points' => :'Float',
        :'health' => :'Float',
        :'is_dead' => :'BOOLEAN',
        :'direction' => :'Float',
        :'speed' => :'Float',
        :'pos_x' => :'Float',
        :'pos_y' => :'Float',
        :'max_board_x' => :'Float',
        :'max_board_y' => :'Float',
        :'cannon_reloading_time' => :'Float',
        :'fired_new_missile' => :'BOOLEAN',
        :'scan_status' => :'ScanStatus'
        
      }
    end

    def initialize(attributes = {})
      return unless attributes.is_a?(Hash)

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'name']
        self.name = attributes[:'name']
      end
      
      if attributes[:'token']
        self.token = attributes[:'token']
      end
      
      if attributes[:'configuration']
        self.configuration = attributes[:'configuration']
      end
      
      if attributes[:'simulationTime']
        self.simulation_time = attributes[:'simulationTime']
      end
      
      if attributes[:'timeTick']
        self.time_tick = attributes[:'timeTick']
      end
      
      if attributes[:'realTimeTick']
        self.real_time_tick = attributes[:'realTimeTick']
      end
      
      if attributes[:'points']
        self.points = attributes[:'points']
      end
      
      if attributes[:'health']
        self.health = attributes[:'health']
      end
      
      if attributes[:'isDead']
        self.is_dead = attributes[:'isDead']
      end
      
      if attributes[:'direction']
        self.direction = attributes[:'direction']
      end
      
      if attributes[:'speed']
        self.speed = attributes[:'speed']
      end
      
      if attributes[:'posX']
        self.pos_x = attributes[:'posX']
      end
      
      if attributes[:'posY']
        self.pos_y = attributes[:'posY']
      end
      
      if attributes[:'maxBoardX']
        self.max_board_x = attributes[:'maxBoardX']
      end
      
      if attributes[:'maxBoardY']
        self.max_board_y = attributes[:'maxBoardY']
      end
      
      if attributes[:'cannonReloadingTime']
        self.cannon_reloading_time = attributes[:'cannonReloadingTime']
      end
      
      if attributes[:'firedNewMissile']
        self.fired_new_missile = attributes[:'firedNewMissile']
      end
      
      if attributes[:'scanStatus']
        self.scan_status = attributes[:'scanStatus']
      end
      
    end

    # Check equality by comparing each attribute.
    def ==(o)
      return true if self.equal?(o)
      self.class == o.class &&
          name == o.name &&
          token == o.token &&
          configuration == o.configuration &&
          simulation_time == o.simulation_time &&
          time_tick == o.time_tick &&
          real_time_tick == o.real_time_tick &&
          points == o.points &&
          health == o.health &&
          is_dead == o.is_dead &&
          direction == o.direction &&
          speed == o.speed &&
          pos_x == o.pos_x &&
          pos_y == o.pos_y &&
          max_board_x == o.max_board_x &&
          max_board_y == o.max_board_y &&
          cannon_reloading_time == o.cannon_reloading_time &&
          fired_new_missile == o.fired_new_missile &&
          scan_status == o.scan_status
    end

    # @see the `==` method
    def eql?(o)
      self == o
    end

    # Calculate hash code according to all attributes.
    def hash
      [name, token, configuration, simulation_time, time_tick, real_time_tick, points, health, is_dead, direction, speed, pos_x, pos_y, max_board_x, max_board_y, cannon_reloading_time, fired_new_missile, scan_status].hash
    end

    # build the object from hash
    def build_from_hash(attributes)
      return nil unless attributes.is_a?(Hash)
      self.class.swagger_types.each_pair do |key, type|
        if type =~ /^Array<(.*)>/i
          if attributes[self.class.attribute_map[key]].is_a?(Array)
            self.send("#{key}=", attributes[self.class.attribute_map[key]].map{ |v| _deserialize($1, v) } )
          else
            #TODO show warning in debug mode
          end
        elsif !attributes[self.class.attribute_map[key]].nil?
          self.send("#{key}=", _deserialize(type, attributes[self.class.attribute_map[key]]))
        else
          # data not found in attributes(hash), not an issue as the data can be optional
        end
      end

      self
    end

    def _deserialize(type, value)
      case type.to_sym
      when :DateTime
        DateTime.parse(value)
      when :Date
        Date.parse(value)
      when :String
        value.to_s
      when :Integer
        value.to_i
      when :Float
        value.to_f
      when :BOOLEAN
        if value.to_s =~ /^(true|t|yes|y|1)$/i
          true
        else
          false
        end
      when :Object
        # generic object (usually a Hash), return directly
        value
      when /\AArray<(?<inner_type>.+)>\z/
        inner_type = Regexp.last_match[:inner_type]
        value.map { |v| _deserialize(inner_type, v) }
      when /\AHash<(?<k_type>.+), (?<v_type>.+)>\z/
        k_type = Regexp.last_match[:k_type]
        v_type = Regexp.last_match[:v_type]
        {}.tap do |hash|
          value.each do |k, v|
            hash[_deserialize(k_type, k)] = _deserialize(v_type, v)
          end
        end
      else # model
        _model = SwaggerClient.const_get(type).new
        _model.build_from_hash(value)
      end
    end

    def to_s
      to_hash.to_s
    end

    # to_body is an alias to to_body (backward compatibility))
    def to_body
      to_hash
    end

    # return the object in the form of hash
    def to_hash
      hash = {}
      self.class.attribute_map.each_pair do |attr, param|
        value = self.send(attr)
        next if value.nil?
        hash[param] = _to_hash(value)
      end
      hash
    end

    # Method to output non-array value in the form of hash
    # For object, use to_hash. Otherwise, just return the value
    def _to_hash(value)
      if value.is_a?(Array)
        value.compact.map{ |v| _to_hash(v) }
      elsif value.is_a?(Hash)
        {}.tap do |hash|
          value.each { |k, v| hash[k] = _to_hash(v) }
        end
      elsif value.respond_to? :to_hash
        value.to_hash
      else
        value
      end
    end

  end
end
