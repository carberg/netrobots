{-
    Copyright 2016 Massimo Zaniboni <massimo.zaniboni@docmelody.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module BoardViewer exposing (..)

import Json.Decode exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html as Html
import Html.Attributes as Html
import Html.App as Html
import AnimationFrame
import Window as W
import Time
import Dict
import Set
import Task as Task
import Platform.Cmd as Cmd
import Http

import Material
import Material.Scheme
import Material.Options exposing (css)
import Material.Progress exposing (progress)
import Material.Grid as Grid
import Material.Table as Table
import Material.Tabs as Tabs

-- ----------------------
-- Utils Functions

valueOr0 : Int -> Int
valueOr0 x = if x < 0 then 0 else x

distanceXYXY : Float -> Float -> Float -> Float -> Float
distanceXYXY x1 y1 x2 y2 =
    let dx : Float
        dx = x2 - x1

        dy : Float
        dy = y2 - y1

        ddx : Float
        ddx = dx ^ 2.0

        ddy : Float
        ddy = dy ^ 2.0
    in sqrt(ddx + ddy)


polarToCartesian : Float -> Float -> Float -> Float -> (Float, Float)
polarToCartesian centerX centerY radius angleInRadians =
    let (x, y) = fromPolar(radius, angleInRadians)
    in  (x + centerX, y + centerY)


-- | Transform an angle in radians in an angle >= 0 and < 2*pi
normalizeUsingPeriod : Float -> Float -> Float
normalizeUsingPeriod period v1 =
    let 
        v2 = v1 - ((toFloat (floor (v1 / period))) * period)
    in  if (v1 >= period) || (v1 < 0.0)
        then if v2 < 0.0 then (period + v2) else v2
        else v1

-- | Transform an angle in radians in an angle >= 0 and < 2*pi
normalizeRad : Float -> Float
normalizeRad rad1 = normalizeUsingPeriod (2.0 * pi) rad1

normalizeDeg : Float -> Float
normalizeDeg d = normalizeUsingPeriod 360.0 d

-- | Nice formatted float, without decimals.
niceFloat : Float -> String
niceFloat n = toString (round n)

toEm : Float -> String
toEm n = (toString n) ++ "em"

-- ------------------------------------
-- Events Received from the Game Server

-- | A game board with a chunk of events happened
--   inside the buffered time-frame.
--   Events are streamed and transferred at chunks
--   so the communication process is more efficient.
type alias BoardInfo =
    {
      maxBoardX : Float
    , maxBoardY : Float
    , streamDelay : Float
    , turnDeltaTime : Float
    , networkLatency : Float
    , startTime : Float
      -- ^ the initial time of the buffer
    , endTime : Float
      -- ^ the end simulation time of the buffer
    , events : List BoardEvent
      -- ^ the events of the buffer
    }

type alias RobotInfo =
    {
      robotId : Int
    , posX : Float
    , posY : Float
    , direction : Float
    , currentSpeed : Float
    , requiredSpeed : Float
    , acceleration : Float
    , reloadingTime : Float
      -- ^ the left time before the robot can fire
    , health : Float
    , points : Float
    , missedTurns : Int
    }

-- | An event to process.
type alias BoardEvent = 
  {
    event : BoardEventVariant
  , activationTime : Float
  }

-- | Events and elements of the board to activate and display.
type BoardEventVariant =
    ECreateRobot EventCreateRobot
  | ERemoveRobot EventRemoveRobot
  | EScan EventScan
  | EMissile EventMissile
  | EExplosion EventExplosion
  | ERobotCollision EventRobotCollision
  | EDrive EventDrive
  | EDisplayRobot EventCreateRobot
  | EDisplayMissile ActiveMissile
  | EDisplayExplosion ActiveExplosion
  | EDisplayScan ActiveScan
  | EDisplayTrack ActiveTrack
  | EDisplayLostPoints ActiveLostPoints

type alias EventCreateRobot =
    {
        robot : RobotInfo
      , name : String
      , color : String
    }

type alias EventRemoveRobot =
    {
         robot : RobotInfo
    }

-- | A track showing the passage of a tank.
type alias ActiveTrack = 
    { deactivationTime : Float
    , lineCords : List (Attribute Msg)
    }

type alias EventScan =
    {
        direction : Float
      , semiaperture : Float
      , scanMaxDistance : Float
      , robot : RobotInfo
      , hitRobot : Maybe RobotInfo
    }

type alias ActiveScan =
    { hitRobot : Bool
    , radarPath : Svg.Attribute Msg
    , radarDebugLine : Svg Msg
    }

type alias EventMissile =
    {
        robot : RobotInfo
      , direction : Float
      , distance : Float
      , speed : Float
    }

type alias ActiveMissile =
    {
        missile : EventMissile
    ,   posX : Float
    ,   posY : Float
    ,   targetX : Float
    ,   targetY : Float
    ,   color : String
    ,   directionDeg : Float
    ,   deactivationTime : Float 
    }

-- | Show an explosion.
type alias ActiveExplosion = 
    { deactivationTime : Float
    , centerX : Float
    , centerY : Float
    , color : String
    }

-- | Show info about lost points, as a message contrary to the tank movement.
type alias ActiveLostPoints = 
    { deactivationTime : Float
    , direction : Float
    , startX : Float
    , startY : Float
    , msg : String
    }

type alias EventExplosion =
    {
        robot : RobotInfo
    ,   hitRobot : RobotInfo
    ,   damage : Float
    }


type alias EventRobotCollision =
    {
        robot : RobotInfo
    }

type alias EventDrive =
    {
      robot : RobotInfo
    }

-- ---------------------------------------
-- Model

type alias RobotId = Int

type alias Seconds = Float

-- | The Elm/view Model.
type alias Model = {

      isInitializated : Bool

    , windowSize : W.Size

    , preStreaming : Seconds
    -- ^ how many seconds are passed in the pre-streaming phase (waiting data)

    , streamingTot : Seconds
    -- ^ total seconds required for the streaming, excluded the `preStreaming` phase.

    , streamingLeft : Seconds
    -- ^ in the streaming phase how many seconds are left

    , simulationIsStarted : Bool
    -- ^ False during initial phases of the simulation when data must be streamed

    , boardInfo : Maybe BoardInfo
    -- ^ current streamed chunk of events

    , streamedBoardInfo : Dict.Dict Seconds BoardInfo
    -- ^ bufferend and streamed chunk of events (BoardInfo)
    --   that must be processed in the future.
    --   It is safe using `Seconds` as key because
    --   it is a buffered chunk of events. 

    , nextEvents : List BoardEvent
    -- ^ events that must be activated/processed in the future.
    --   Events of the past are removed.

    , activeEvents : List BoardEvent
    -- ^ active events and elements to display on the board

    , robotInfo : Dict.Dict RobotId (Seconds, EventCreateRobot)
    -- ^ the current status of every robotInfo.

    , usedRobotColors : Dict.Dict String String
    -- ^ map a robot color to an identifier used for gradients and similar things.

    , currentSimulationTime : Seconds

    , errorMessages : List String

    , totRedrawFrames : Int

    , totSeconds : Seconds

    , mdl : Material.Model

    }

boardInfo_fromRealTimeToSimulatedTime : BoardInfo -> Seconds -> Float
boardInfo_fromRealTimeToSimulatedTime bi s = (bi.turnDeltaTime / bi.networkLatency) * s

model_robotColor : Model -> RobotId -> String
model_robotColor m id =
    case Dict.get id (m.robotInfo) of
      Nothing -> Debug.crash "unexpeced error in the code: 1053"
      Just (_, i) -> i.color

-- | Given a robot color return the correspondig gradient identifier to use.
--   Use yellow as fallback color.
model_fromColorToExplosionGradientId : Model -> String -> String
model_fromColorToExplosionGradientId m c =
          case Dict.get c (m.usedRobotColors) of
            Nothing -> "red" 
            Just i -> "url(#" ++ i ++ ")"

-- -----------------------------------------
-- Init

init : (Model, Cmd Msg)
init =
    let m = {
          isInitializated = False
        , windowSize = { width = 0, height = 0 }
        , preStreaming = 0.0
        , streamingTot = 0.0
        , streamingLeft = 0.0
        , simulationIsStarted = False
        , boardInfo = Nothing
        , streamedBoardInfo = Dict.empty
        , nextEvents = []
        , robotInfo = Dict.empty
        , usedRobotColors = Dict.empty 
        , activeEvents = []
        , currentSimulationTime = 0.0
        , errorMessages = []
        , totRedrawFrames = 0
        , totSeconds = 0.0
        , mdl = Material.model
        }

        askWindowSize =
            Task.perform
              (\x -> MsgWindowSize { width = 0, height = 0})
              (\s -> MsgWindowSize s)
              W.size
    in (m, Cmd.batch [cmd_make MsgInitBoard, askWindowSize]) 

cmd_make : msg -> Cmd msg
cmd_make msg =
  Task.perform identity identity (Task.succeed msg)

main : Program Never
main =
    Html.program {
            init = init
        ,   update = update
        ,   subscriptions = subscriptions
        ,   view = view
        }

-- ---------------------------------------
-- Model Updates

type Msg =
    MsgAdvanceRealTime Seconds
  | MsgFetchFail String
  | MsgInitBoard
  | MsgStreamBoardInfo BoardInfo
  | MsgWindowSize W.Size
  | Mdl (Material.Msg Msg)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    AnimationFrame.diffs (\time -> MsgAdvanceRealTime (Time.inSeconds time) )
  , W.resizes (\s -> MsgWindowSize s)
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model1 =
    case msg of
        MsgInitBoard
            -> (model1, fetchBoardInfo True)

        MsgStreamBoardInfo s
            -> case model1.isInitializated of
                   False -> (model_init model1 s, fetchBoardInfo False)
                   True ->  (model_addStream model1 s, fetchBoardInfo False)

        MsgFetchFail s -> ({model1 | errorMessages = List.take 50 (model1.errorMessages ++ [s])}
                          , fetchBoardInfo (not (model1.isInitializated)))

        MsgAdvanceRealTime s ->
            case model1.simulationIsStarted of
                False -> case model1.isInitializated of
                             True ->  ({model1 | streamingLeft = model1.streamingLeft - s }, Cmd.none)
                             False -> ({ model1 | preStreaming = model1.preStreaming + s}, Cmd.none)
                True -> (model_advance model1 s, Cmd.none)

        MsgWindowSize s -> ({ model1 | windowSize = s }, Cmd.none)

        Mdl msg' -> Material.update msg' model1

fetchBoardInfo : Bool -> Cmd Msg
fetchBoardInfo isInit =
    let onHttpError err = toString err
        url = if isInit then "/board-info" else "/board-events"
    in  Task.perform MsgFetchFail MsgStreamBoardInfo (Task.mapError onHttpError (Http.get boardInfoDecoder url))

model_init : Model -> BoardInfo -> Model
model_init m1 bi =
    let initRobot e m =
            case e.event of
                (ECreateRobot _) -> model_processEvent m e
                _ -> m

        ts = bi.streamDelay * 2.0

        m2 = {m1 | isInitializated = True
                 , streamingLeft = ts
                 , streamingTot = ts
                 , boardInfo = Just bi
                 , currentSimulationTime = bi.startTime
             }

    in List.foldl initRobot m2 bi.events

model_addStream : Model -> BoardInfo -> Model
model_addStream m bi =
    {m | 
         streamedBoardInfo =  Dict.insert (bi.startTime) bi m.streamedBoardInfo
       , simulationIsStarted = m.simulationIsStarted || ((Dict.size m.streamedBoardInfo) >= 1)
    }

-- | Advance the simulation of the model.
--   This is the entry point function to call.
model_advance : Model -> Seconds -> Model
model_advance m1 deltaRealTimeSeconds =
    let (m2, canProcess) = model_loadStreamedData m1
    in if canProcess then model_processAllEvents m2 deltaRealTimeSeconds else m2

-- | Return True if the data can be processed, or False if more streamed data is required.
model_loadStreamedData : Model -> (Model, Bool)
model_loadStreamedData m1 =
    -- first advance m1.streamedBoardInfo if it is the case
    case m1.boardInfo of
       Nothing ->
           case List.head (Dict.keys m1.streamedBoardInfo) of
               Nothing ->
                   (m1, False)
                   -- do nothing, waiting for new streamed models from the server
               Just minKey ->
                   case Dict.get minKey m1.streamedBoardInfo of
                       Nothing ->
                           (m1, False)
                       Just newBoardInfo ->
                           ({m1|  boardInfo = Just newBoardInfo
                                , streamedBoardInfo = Dict.remove minKey m1.streamedBoardInfo
                                ,    nextEvents = newBoardInfo.events
                            }, True)
                           -- load new bufferend events and process them

       Just bi -> (m1, True)
                  -- there is already data to process

-- | Advance the simulation of the model processing all the events.
--   The big picture is:
--   * streamed events are put in the list of events to process
--   * events from the server are transformed to `Model.activeEvents` to display on the board
--   * the `viewContent` function will use this info for displaying the board
--   @require there is a `Model.boardInfo` to process
model_processAllEvents : Model -> Seconds -> Model
model_processAllEvents m1 deltaRealTimeSeconds =
  case m1.boardInfo of
    Nothing -> m1
    Just bi -> 
     let
       deltaTime = boardInfo_fromRealTimeToSimulatedTime bi deltaRealTimeSeconds
       m2 = { m1 | currentSimulationTime = m1.currentSimulationTime + deltaTime
                 , totRedrawFrames = m1.totRedrawFrames + 1
                 , totSeconds = m1.totSeconds + deltaRealTimeSeconds 
            }

       m3 = model_processActiveEvents m2
       m4 = model_processNextEvents m3
       m5 = model_addRobotEvents m4

  in m5

-- | Process every active event.
model_processActiveEvents : Model -> Model
model_processActiveEvents m1 = 
  let m0 = { m1 | activeEvents = [] }
  in  List.foldl (\ e m -> model_processEvent m e) m0 m1.activeEvents

-- | Process `Model.nextEvents`.
model_processNextEvents : Model -> Model
model_processNextEvents m1 =
  case m1.boardInfo of
    Nothing
      -> m1 -- no buffered info to process
    Just bi
      -> case List.head m1.nextEvents of
           Nothing ->
             case m1.currentSimulationTime > bi.endTime of
               True -> let (m2, _) = model_loadStreamedData { m1 | boardInfo = Nothing }
                       in  model_processNextEvents m2
                       -- load also the next buffered events
               False -> m1
                        -- process next events later, when the simulation reach them 

           Just event ->
             case event.activationTime <= m1.currentSimulationTime of
               False -> m1
                        -- process next events later, when the simulation reach them 
               True -> let m2 = { m1 | nextEvents = case List.tail m1.nextEvents of
                                                      Nothing -> []
                                                      Just r -> r
                                }
                           m3 = model_processEvent m2 event
                       in  model_processNextEvents m3

-- | Process the event, updating `Model.activeEvents`,
--   and `Model.robotInfo`.
--   The event can be a new event from `Model.nextEvents`
--   or an already existing event from `Model.activeEvents`.
--   If the event is not put again in `Model.activeEvents`
--   it will be removed from the system.
--   @require `be` is an event applicable in the time-frame.
model_processEvent : Model -> BoardEvent-> Model
model_processEvent m1 be =
    let
        currentTime = m1.currentSimulationTime
        activationTime = be.activationTime

        robotInfo : RobotId -> (Seconds, EventCreateRobot)
        robotInfo robotId =
            case Dict.get robotId m1.robotInfo of
                Nothing -> Debug.crash "impossible"
                Just r -> r

        updateRobot : Model -> RobotInfo -> Model
        updateRobot m r =
               let 
                   (_, ri) = robotInfo r.robotId
               in  { m | robotInfo = Dict.insert r.robotId (be.activationTime, {ri | robot = r }) m1.robotInfo } 

        updateMaybeRobot : Model -> Maybe RobotInfo -> Model
        updateMaybeRobot m mr =
            case mr of
                Just r -> updateRobot m r
                Nothing -> m

        addActiveBoardEvent : Model -> BoardEvent -> Model
        addActiveBoardEvent m1 be = { m1 | activeEvents = be::(m1.activeEvents) }

        addActiveEvent : Model -> BoardEventVariant -> Model
        addActiveEvent m1 e = addActiveBoardEvent m1 { be | event = e } 

        initActiveMissile : EventMissile -> ActiveMissile
        initActiveMissile missile =
            let 
                deltaTime = currentTime - activationTime
                speed = missile.speed

                movement = speed * deltaTime

                cosX = cos (degrees missile.direction)
                sinY = sin (degrees missile.direction)
                -- NOTE: degrees in Elm convert from degrees to radians

                dx = movement * cosX 
                dy = movement * sinY

                posX = missile.robot.posX + dx
                posY = missile.robot.posY + dy

                targetX = missile.robot.posX + missile.distance * cosX 
                targetY = missile.robot.posY + missile.distance * sinY

                timeToTarget = missile.distance / speed

                deactivationTime = activationTime + timeToTarget

            in  {   missile = missile
                ,   posX = posX
                ,   posY = posY
                ,   targetX = targetX
                ,   targetY = targetY
                ,   directionDeg = missile.direction
                ,   color = model_robotColor m1 missile.robot.robotId
                ,   deactivationTime = deactivationTime
                }

        updateActiveMissile : ActiveMissile -> ActiveMissile
        updateActiveMissile missile = initActiveMissile (missile.missile)

        initActiveExplosion : ActiveMissile -> ActiveExplosion
        initActiveExplosion missile =
            { deactivationTime = currentTime + 2.0
            , centerX = missile.targetX
            , centerY = missile.targetY
            , color = missile.color
            }

        initActiveLostPoints : EventExplosion -> ActiveLostPoints
        initActiveLostPoints e =
            let msg = "-" ++ niceFloat e.damage
            in { deactivationTime = currentTime + 5.0
               , startX = e.hitRobot.posX
               , startY = e.hitRobot.posY
               , direction = normalizeDeg (e.hitRobot.direction + 180.0)
               , msg = msg
               }

        initActiveScan : EventScan -> ActiveScan
        initActiveScan scan =
            let

                piD2 = pi / 2.0

                centerX : Float
                centerX = scan.robot.posX

                centerY : Float
                centerY = scan.robot.posY

                directionRad = (normalizeRad (degrees scan.direction))
                semiapertureRad = (Basics.min pi (normalizeRad (degrees scan.semiaperture)))

                (hasRecognizedSomething, distance, targetX, targetY)
                     = case scan.hitRobot of
                           Nothing -> (False
                                      , scan.scanMaxDistance
                                      , centerX + scan.scanMaxDistance * (cos directionRad)
                                      , centerY + scan.scanMaxDistance * (sin directionRad)
                                      )
                           Just r -> (True
                                     , distanceXYXY centerX centerY r.posX r.posY
                                     , r.posX
                                     , r.posY
                                     )

                (startAngleRad, endAngleRad) = (normalizeRad (directionRad - semiapertureRad), normalizeRad (directionRad + semiapertureRad))
                (startX, startY) = (polarToCartesian centerX centerY distance startAngleRad)
                (endX, endY) = (polarToCartesian centerX centerY distance endAngleRad)
                (directionX, directionY) = (polarToCartesian centerX centerY distance directionRad)

                largeArcFlag = if (semiapertureRad > piD2) then "1" else "0"

                isLimitCase = distance <= 0.1

                radarPath = if abs (semiapertureRad - pi) > 0.02
                            then Svg.Attributes.d
                                  ("M " ++ toString centerX  ++ " " ++ toString centerY
                                   ++ " L " ++ toString startX ++ " " ++ toString startY
                                   ++ " A " ++ toString distance ++ " " ++ toString distance ++ " 0 " ++ largeArcFlag ++ " 1 " ++ toString endX ++ " " ++ toString endY
                                   ++ " L " ++ toString centerX ++ " " ++ toString centerY
                                   ++ " Z")
                            else Svg.Attributes.d
                                  ("M " ++ toString centerX  ++ " " ++ toString centerY
                                   ++ " m -" ++ toString distance ++ " 0"
                                   ++ " a " ++ toString distance ++ " " ++ toString distance ++ " 0 1 1 " ++ (toString (distance * 2.0)) ++ " 0"
                                   ++ " a " ++ toString distance ++ " " ++ toString distance ++ " 0 1 1 -" ++ (toString (distance * 2.0)) ++ " 0"
                                  )
                                  -- NOTE: SVG can not draw an arc that is "near" a circle, so simulate a circle with this trick.

                targetLineColor = if hasRecognizedSomething then "red" else "blue"
                radarDebugLine
                    = Svg.g []
                            [
                               Svg.line [ x1 (toString centerX), y1 (toString centerY), x2 (toString targetX), y2 (toString targetY), stroke targetLineColor] []
                             , Svg.line [ x1 (toString centerX), y1 (toString centerY), x2 (toString directionX), y2 (toString directionY), stroke "blue"] []
                            ]

            in  { hitRobot = hasRecognizedSomething
                , radarPath = if isLimitCase then Svg.Attributes.d "" else radarPath
                , radarDebugLine = if isLimitCase then Svg.g [] [] else radarDebugLine
                }

    in case be.event of
           ECreateRobot e -> { m1 | robotInfo = Dict.insert e.robot.robotId ( be.activationTime, e) m1.robotInfo
                                  , usedRobotColors = (Dict.insert e.color ("id_" ++ toString (List.length (Dict.keys m1.usedRobotColors))) m1.usedRobotColors)
                             }

           ERemoveRobot e -> { m1 | robotInfo = Dict.remove e.robot.robotId m1.robotInfo }
           -- MAYBE add also an event about some robot explosion,
           -- and info that the robot is dead

           EDrive e -> updateRobot m1 e.robot

           EScan e -> addActiveEvent (updateMaybeRobot (updateRobot m1 e.robot) e.hitRobot) (EDisplayScan (initActiveScan e))

           EDisplayScan e
               -> m1

           EMissile e ->  addActiveEvent
                            (updateRobot m1 e.robot)
                            (EDisplayMissile (initActiveMissile e))

           EDisplayMissile e
               -> if e.deactivationTime <= currentTime
                  then addActiveEvent m1 (EDisplayExplosion (initActiveExplosion e))
                  else addActiveEvent m1 (EDisplayMissile (updateActiveMissile e))

           EDisplayExplosion e
               -> if e.deactivationTime <= currentTime
                  then m1
                  else addActiveBoardEvent m1 be

           EDisplayTrack e
               -> if e.deactivationTime <= currentTime
                  then m1
                  else addActiveBoardEvent m1 be 

           EDisplayLostPoints e
               -> if e.deactivationTime <= currentTime
                  then m1
                  else addActiveBoardEvent m1 be 

           EExplosion e ->
               let m2 = updateRobot m1 e.robot
                   m3 = updateRobot m2 e.hitRobot
                   m4 = addActiveEvent m3 (EDisplayLostPoints (initActiveLostPoints e))
               in m4

           ERobotCollision e -> updateRobot m1 e.robot
           _ -> m1

-- | Update `Model.activeEvents` with info from `Model.robotInfo`.
model_addRobotEvents : Model -> Model
model_addRobotEvents model =
  let finalModel = { model | activeEvents = finalEvents }

      simulationTime = model.currentSimulationTime

      -- | True for enabling the track effect
      enableTrack = False

      -- | The frequency of tracks.
      leaveTrackEvery = 0.5

      -- | How much time the track remain visible.
      trackPermanence = 20.0

      isTimeToLeaveTheTrack deltaTime =
          let st0 = simulationTime - deltaTime
              st1 = simulationTime

              cycle : Float -> Int
              cycle v = truncate (v / leaveTrackEvery)
          in if enableTrack then (cycle st0) < cycle st1 else False 

      finalEvents : List BoardEvent
      finalEvents = Dict.foldl
                      (\_ (activationTime, ecr) events1
                               -> let (robotEvent, activeTrack) = processRobot (activationTime, ecr)
                                      deltaTime = simulationTime - activationTime
                                      events2 =
                                        case isTimeToLeaveTheTrack deltaTime of
                                          True -> ({activationTime = activationTime, event = EDisplayTrack activeTrack}) :: events1
                                          False -> events1 
                                      events3 = ({ activationTime = activationTime, event = robotEvent }) :: events2
                                  in events3
                      ) model.activeEvents model.robotInfo

      processRobot : (Seconds, EventCreateRobot) -> (BoardEventVariant, ActiveTrack)
      processRobot (activationTime, ecr) =
        let 
            deltaTime = simulationTime - activationTime

            deltaTimeToRequiredSpeed = (ecr.robot.requiredSpeed - ecr.robot.currentSpeed) / ecr.robot.acceleration

            robotInfo = ecr.robot

            (deltaTimeWithNormalAcceleration, deltaTimeWithMaxSpeed, currentAcceleration)
               = if deltaTime > deltaTimeToRequiredSpeed
                 then (deltaTimeToRequiredSpeed, deltaTime - deltaTimeToRequiredSpeed, 0.0)
                 else (deltaTime, 0, ecr.robot.acceleration)

            movement1 = ecr.robot.currentSpeed * deltaTimeWithNormalAcceleration + (0.5 * ecr.robot.acceleration * (deltaTimeWithNormalAcceleration * deltaTimeWithNormalAcceleration))
            movement2 = ecr.robot.requiredSpeed * deltaTimeWithMaxSpeed
            movement = movement1 + movement2

            dy = movement * (sin (degrees ecr.robot.direction))
            dx = movement * (cos (degrees ecr.robot.direction))
            -- NOTE: degrees in Elm convert from degrees to radians

            posX = ecr.robot.posX + dx
            posY = ecr.robot.posY + dy

            direction = ecr.robot.direction
            thereIsMirror = (direction >= 0.0 && direction <= 90.0) || direction >= 270.0

            reloadingTime1 = ecr.robot.reloadingTime - deltaTime
            reloadingTime2 = if reloadingTime1 < 0.0 then 0.0 else reloadingTime1

            track = { deactivationTime = simulationTime + trackPermanence
                    , lineCords = [ x1 (toString posX), y1 (toString posY), x2 (toString (posX + 1.0)), y2 (toString (posY + 1.0)), stroke ecr.color]
                    } 

        in ( EDisplayRobot { ecr | robot = { robotInfo | posX = posX, posY = posY, reloadingTime = reloadingTime2 } }
           , track
           )
  in finalModel

-- ----------------------------------------
-- From Model to View 

type alias Mdl = Material.Model

-- | The view entry point.
view : Model -> Html.Html Msg
view model = 
    let isThereBoardSize = not (model.windowSize.width == 0)
        isThereBoard = case model.boardInfo of
                           Nothing -> False
                           Just _ -> True

    in if model.simulationIsStarted && isThereBoardSize && isThereBoard then viewContent model else viewStreaming model


-- | Display the initial streaming message.
viewStreaming : Model -> Html.Html Msg
viewStreaming model =
  let 

      calcPerc : Float -> Float -> Float -> Float
      calcPerc completed total totalPerc = completed * totalPerc / (total * totalPerc / 100.0)

      completitionPerc : Bool -> Float
      completitionPerc isPrestreaming =
          if isPrestreaming
          then calcPerc model.preStreaming 3.0 33
          -- NOTE: consider that the mean pre streaming phase require 3 seconds,
          -- and it is long the 33% of the total time
          else let preStreamingPerc = completitionPerc True
                   leftPerc = 100.0 - preStreamingPerc
                   streamingPerc = calcPerc (model.streamingTot - model.streamingLeft) model.streamingTot leftPerc
               in streamingPerc + preStreamingPerc

      perc = Basics.min 100.0 (completitionPerc (not (model.isInitializated)))

      titleSize = [Grid.offset Grid.Desktop 3, Grid.size Grid.Desktop 6
                  , Grid.offset Grid.Tablet 1, Grid.size Grid.Tablet 6
                  , Grid.size Grid.Phone 4]

  in  Html.div []
        [ 
          Grid.grid
            []
            [  Grid.cell titleSize [ netRobotsLogo "100%" "100%"]
             , Grid.cell titleSize [ progress perc]
            ]
        ]

-- | Display the game board.
--   NOTE: the majority of screen have more horizontal space than vertical,
--   and usually boards are square. So the info section on the left uses
--   free screen space.
--   @require model.boardInfo is Just
viewContent : Model -> Html.Html Msg
viewContent model =
    let horizontalBorder = 4.0
        verticalBorder = 4.0
        svgPerc = 0.8
        labelPerc = 1.0 - svgPerc
        svgWidth = (toFloat model.windowSize.width) * svgPerc - verticalBorder * 2
        svgHeight = (toFloat model.windowSize.height) - horizontalBorder * 2
        labelWidth = (toFloat model.windowSize.width) * labelPerc - verticalBorder * 2
        labelHeight = svgHeight

        toPx : Float -> String
        toPx n = toString (truncate n) ++ "px"

        svgViewBox
            = "0 0 " ++ (toString boardInfo.maxBoardX) ++ " " ++ (toString boardInfo.maxBoardY)

        boardInfo =
            case model.boardInfo of
                Just r -> r
                Nothing -> Debug.crash "contract not respected"

        containerCssStyle : Attribute Msg
        containerCssStyle = 
         Html.style [
              ("display", "flex")
             ,("flex-flow", "row")
             ,("flex-grow", "0")
             ,("flex-wrap","nowrap")
             ,("justify-content", "flex-start")
             ]

        cssStyle : Float -> Float -> Attribute Msg
        cssStyle width height  =
            Html.style [
                 ("width",  toPx width)
                ,("height", toPx height)
                ,("margin", "auto")
                ]

    in  Html.div [ containerCssStyle ] [
              Html.div [ cssStyle svgWidth svgHeight ] [
                   svg [ version "1.1"
                       , x "0", y "0"
                       , width (toPx svgWidth), height  (toPx svgHeight)
                       , preserveAspectRatio "xMinYMin meet"
                       , viewBox svgViewBox
                       ] (viewBoard model)
                   ]
              , Html.div [cssStyle labelWidth labelHeight] [viewInfoSection model]
        ]

-- | Show game info.
viewInfoSection : Model -> Html.Html Msg
viewInfoSection model =
    let 
        viewErrorMessages =
            [Html.ul [] (List.map (\msg -> Html.li [] [Html.text msg]) (List.take 10 model.errorMessages))]

        bi = case model.boardInfo of
                 Just v -> v
                 _ -> Debug.crash "unexpected error 5757"

        fps = if model.totSeconds == 0.0 then 0.0 else (toFloat model.totRedrawFrames) / model.totSeconds

        viewServerInfo : Html.Html Msg
        viewServerInfo =
            Table.table [] [
               Table.thead []
                  [ Table.tr []
                        [ Table.th [ ] [ text "Server Property" ]
                        , Table.th [ ] [ text "Value" ]
                        ]
                  ]
             , Table.tbody []
                           [ Table.tr []
                                [ Table.td [ ] [ text "Game Board dim." ]
                                , Table.td [ ] [ text ((niceFloat bi.maxBoardX) ++ " x " ++ (niceFloat bi.maxBoardY)) ]
                                ]
                           , Table.tr []
                                [ Table.td [ ] [ text "Robot moves for each second" ]
                                , Table.td [ Table.numeric ] [ text (toString (1.0 / bi.turnDeltaTime)) ]
                                ]
                           , Table.tr []
                                [ Table.td [ ] [ text "Virtual simulated seconds" ]
                                , Table.td [ Table.numeric ] [ text (niceFloat model.currentSimulationTime) ]
                                ]
                           , Table.tr []
                                [ Table.td [ ] [ text "Max network latency (in seconds))" ]
                                , Table.td [ Table.numeric ] [ text (toString bi.networkLatency) ]
                                ]
                           , Table.tr []
                                [ Table.td [ ] [ text "Frame per Second" ]
                                , Table.td [ Table.numeric ] [ text (niceFloat fps) ]
                                ]
                            ]
                ]

        viewRobotsInfo =
            Table.table [] [
               Table.thead []
                  [ Table.tr []
                        [ Table.th [ ] [ text "Robot" ]
                        , Table.th [ ] [ text "Points" ]
                        , Table.th [ ] [ text "Health" ]
                        , Table.th [ ] [ text "Missed Turns"]
                        ]
                  ]
             , Table.tbody []
                ((Dict.values model.robotInfo) |> List.map (\(_, ecr) ->
                                       Table.tr []
                                       [ Table.td [ css "color" ecr.color] [ text ecr.name ]
                                       , Table.td [ Table.numeric ] [ text (niceFloat ecr.robot.points) ]
                                       , Table.td [ Table.numeric ] [ text (niceFloat ecr.robot.health) ]
                                       , Table.td [ Table.numeric ] [ text (toString ecr.robot.missedTurns) ]
                                        ]))
            ]

        fullSize = [ Grid.size Grid.All 12 ]

        emptyRow = Grid.cell fullSize [ Html.p [] []]

    in Grid.grid
            [  Grid.noSpacing ]
            [  Grid.cell fullSize [ netRobotsLogo "100%" "100%"]
             , Grid.cell fullSize [ viewRobotsInfo ]
             , emptyRow
             , Grid.cell fullSize [ viewServerInfo ]
            ]


-- | Display Game Board.
viewBoard : Model -> List (Svg Msg)
viewBoard model =
    let boardInfo
          = case model.boardInfo of
               Just r -> r
               Nothing -> Debug.crash "contract not respected"

        currentTime = model.currentSimulationTime

        toPx : Float -> String
        toPx n = toString (truncate n) ++ "px"

        boardMaxX = boardInfo.maxBoardX
        boardMaxY = boardInfo.maxBoardY

        drawBoardPerimeter =
            rect [width (toPx boardMaxX), height (toPx boardMaxY), stroke "Black", fill "GhostWhite", strokeWidth "1px"] []

        drawActiveEvents = List.foldl (\e l -> drawActiveEvent l e) [] model.activeEvents

        drawActiveEvent results1 be =
            case be.event of
                EDisplayMissile e -> (drawMissile be.activationTime e)::results1
                EDisplayRobot e -> (drawRobot e)::results1
                EDisplayScan e -> (drawScan be.activationTime e)::results1
                EDisplayTrack e -> (drawTrack be.activationTime e)::results1
                EDisplayExplosion e -> (drawExplosion be.activationTime e)::results1
                EDisplayLostPoints e -> (drawLostPoints be.activationTime e)::results1
                _ -> results1

        drawTrack : Float -> ActiveTrack -> Svg Msg
        drawTrack activationTime e =
            let opacity = Basics.max 0.0 (1.0 - (currentTime - activationTime) / (e.deactivationTime - activationTime))
            in  Svg.line (e.lineCords ++ [Svg.Attributes.strokeOpacity (toString opacity)]) []  

        drawExplosion : Float -> ActiveExplosion -> Svg Msg
        drawExplosion activationTime e =
            let opacity = Basics.max 0.0 (1.0 - (currentTime - activationTime) / (e.deactivationTime - activationTime))
            in  Svg.circle [ cx (toString e.centerX)
                           , cy (toString e.centerY)
                           , r (toString 45.0)
                           , fill (model_fromColorToExplosionGradientId model e.color)
                           , fillOpacity (toString opacity)
                           ] []

        drawLostPoints : Float -> ActiveLostPoints -> Svg Msg
        drawLostPoints activationTime e =
            let opacity = Basics.max 0.0 (1.0 - (currentTime - activationTime) / (e.deactivationTime - activationTime))
                deltaTime = currentTime - activationTime
                speed = 8.0
                posX = e.startX + deltaTime * speed * cos(degrees e.direction)
                posY = e.startY + deltaTime * speed * sin(degrees e.direction)

            in Svg.text' [x (toString posX), y (toString posY), fill "blue"] [Svg.text e.msg]

        drawRobot : EventCreateRobot -> Svg Msg
        drawRobot ecr =
            let
                transformParams = "scale(-1,1)"

                robotStatus = (niceFloat ecr.robot.points) ++ "/" ++ (niceFloat ecr.robot.health)

            in Svg.g [] [
                  use [fill ecr.color
                      , x (toPx ecr.robot.posX)
                      , y (toPx ecr.robot.posY)
                      , xlinkHref "#tank"
                      , width (toEm tankWidthEm)
                      , height (toEm tankHeightEm)
                      ] []
                , Svg.text' [x (toPx ecr.robot.posX), y (toPx ecr.robot.posY), fill "black"]
                    [ Svg.tspan [dy (toEm (tankHeightEm + 0.5))]
                          [Svg.text (ecr.name ++ ": " ++ robotStatus)]
                    ]
                ]

        drawMissile : Float -> ActiveMissile -> Svg Msg
        drawMissile activationTime missile =
            use [ fill missile.color
                , stroke "black"
                , xlinkHref "#missile"
                , x (toPx missile.posX)
                , y (toPx missile.posY)
                , width (toEm missileWidthEm), height (toEm missileHeightEm)
                , transform ("rotate(" ++ toString (normalizeDeg (missile.directionDeg - 180.0)) ++ "," ++ toString (missile.posX + missileWidthPx / 2.0) ++ "," ++ toString (missile.posY + missileHeightPx / 2.0) ++ ")")
                ] []

        drawScan : Float -> ActiveScan -> Svg Msg
        drawScan activationTime scan =
          let opacity1 = 1.0
              opacity2 = toString 1.0
              fillColor = "#6464FF"
          in Svg.g []
              [
               Svg.path
                 [ scan.radarPath
                 , fill fillColor
                 , strokeWidth "1"
                 , stroke fillColor
                 , opacity opacity2
                 , fillOpacity opacity2
                 , strokeOpacity opacity2
                 ] []
              , scan.radarDebugLine
              ]

    in  (Svg.defs [] ((colorGradientDefs model.usedRobotColors) ++ [tankSymbolDef, missileSymbolDef])) :: drawBoardPerimeter :: drawActiveEvents 

colorGradientDefs : Dict.Dict String String -> List (Svg Msg)
colorGradientDefs colorToId =
    List.map (\ (color, colorId) ->
       let boardColor = "white"  
       in Svg.radialGradient
            [ id colorId]
            [ stop [offset "4%", stopColor color] []
            , stop [offset "96%", stopColor boardColor] []]
           -- NOTE the center is the 2 unit of measure with 100% damage,
           -- and the rest is the 45 units with linear decreasing damage.
           -- The proporiotn is 4%
             ) (Dict.toList colorToId)


tankHeightEm = 2.0
tankWidthEm = 4.0

tankSymbolDef : Svg Msg
tankSymbolDef =
   symbol [id "tank", preserveAspectRatio "xMinYMin meet", width (toEm tankWidthEm), height (toEm tankHeightEm), viewBox "0 0 296 582"] [
        g [
    transform "matrix(1.8,0,0,1.8,-108.63617,-967.4266)"
  , id "tank"] [
    Svg.path [
      d "m 290.61109,594.7957 -70.62176,0 -21.67599,-11.65375 0,-32.8636 138.91278,-12.81913 0,42.88583 -46.61503,9.08992 0,5.36073 z"
    , id "path5534"] []
  , Svg.path [
      d "m 65.22745,640.14176 318.38061,0.56974 -0.0533,-59.20108 -91.31212,20.04446 -132.85282,0 -94.16235,25.17211 c -5.54704,2.24346 -7.3857,13.41477 0,13.41477 z"
    , Svg.Attributes.style "display:inline;overflow:visible;visibility:visible;fill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:1;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-start:none;marker-mid:none;marker-end:none"
    , id "path5536"] []
  , Svg.path [
      d "m 155.81493,666.86299 c -3e-5,11.30705 -9.15117,20.48115 -20.45818,20.50952 -11.30701,0.0284 -20.50407,-9.09969 -20.56084,-20.4066 -0.0568,-11.3069 9.04816,-20.52686 20.35489,-20.61203 11.30672,-0.0852 20.54952,8.99658 20.66309,20.30305"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1874"] []
  , Svg.path [
      d "m 201.97733,666.86299 c -3e-5,11.30705 -9.15117,20.48115 -20.45818,20.50952 -11.30701,0.0284 -20.50407,-9.09969 -20.56084,-20.4066 -0.0568,-11.3069 9.04816,-20.52686 20.35489,-20.61203 11.30672,-0.0852 20.54952,8.99658 20.66309,20.30305"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1876"] []
  , Svg.path [
      d "m 247.33033,666.86299 c -3e-5,11.30705 -9.15117,20.48115 -20.45818,20.50952 -11.30701,0.0284 -20.50407,-9.09969 -20.56084,-20.4066 -0.0568,-11.3069 9.04816,-20.52686 20.35489,-20.61203 11.30672,-0.0852 20.54952,8.99658 20.66309,20.30305"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1880"] []
  , Svg.path [
      d "m 293.76333,666.86299 c -3e-5,11.30705 -9.15117,20.48115 -20.45818,20.50952 -11.30701,0.0284 -20.50407,-9.09969 -20.56084,-20.4066 -0.0568,-11.3069 9.04816,-20.52686 20.35489,-20.61203 11.30672,-0.0852 20.54952,8.99658 20.66309,20.30305"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path3511"] []
  , Svg.path [
      d "m 83.75265,646.62921 -9.5625,0.4375 c -5.32566,13.2481 -0.88992,25.37037 13.75,34.34375 16.49748,10.11194 36.32098,19.84936 42.0625,20 l 195.0625,0.0625 c 5.74152,-0.15064 25.56503,-9.88804 42.0625,-20 14.63992,-8.97338 19.07566,-21.1269 13.75,-34.375 l -9.5625,-0.40625 c 2.87764,12.39545 5.36125,20.76159 -12.5625,29.25 -14.66994,6.94744 -30.17637,15.21816 -32.59375,15.71875 l -197.25,-0.0312 c -2.41737,-0.50061 -17.89256,-8.80256 -32.5625,-15.75 -17.92375,-8.4884 -15.47139,-16.85455 -12.59375,-29.25 z m -23.30406,-90.04305 0,13.58752 133.19884,4.52917 0,-18.32256 -133.19884,0.20587 z"
    , Svg.Attributes.style "display:inline;overflow:visible;visibility:visible;fill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:1;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-start:none;marker-mid:none;marker-end:none"
    , id "path1890"] []
  , Svg.path [
      d "m 339.83623,667.93559 c -3e-5,11.30705 -9.15117,20.48115 -20.45818,20.50952 -11.30701,0.0284 -20.50407,-9.09969 -20.56084,-20.4066 -0.0568,-11.3069 9.04816,-20.52686 20.35489,-20.61203 11.30672,-0.0852 20.54952,8.99658 20.66309,20.30305"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1884"] []
  , Svg.path [
      d "m 368.27196,658.40424 c -2e-5,6.44528 -5.21638,11.67474 -11.66165,11.69091 -6.44527,0.0162 -11.68781,-5.18704 -11.72017,-11.63225 -0.0324,-6.4452 5.15767,-11.7008 11.60277,-11.74935 6.44511,-0.0486 11.71372,5.12827 11.77846,11.57323"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1886"] []
  , Svg.path [
      d "m 110.01126,658.40424 c -2e-5,6.44528 -5.21638,11.67474 -11.66165,11.69091 -6.44527,0.0162 -11.68781,-5.18704 -11.72017,-11.63225 -0.03236,-6.4452 5.15767,-11.7008 11.60277,-11.74935 6.44511,-0.0486 11.71372,5.12827 11.77846,11.57323"
    , Svg.Attributes.style "fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    , id "path1888"] []]

            ]

missileWidthEm : Float
missileWidthEm = 1.5

missileHeightEm : Float 
missileHeightEm = 1.5

missileWidthPx : Float
missileWidthPx = 50

missileHeightPx : Float
missileHeightPx = 28

missileSymbolDef : Svg Msg
missileSymbolDef =
   symbol [id "missile", preserveAspectRatio "xMinYMin meet", viewBox "0 0 50 28"] [
    g [
      transform "matrix(1.077 0 0 1.0426 -88.42 -61.208)"] [
      g [
        transform "translate(0,50)"] [
        g [
          stroke "#000"
        , strokeWidth "2"
        ] [
          Svg.path [
            d "m83.083 22.775s2.5554-5.5096 5.9192-5.5096h3.3638"] []
        , Svg.path [
            d "m83.008 22.624s2.5554 5.5096 5.9192 5.5096h3.3638"] []
        , Svg.path [
            d "m89.207 17.266 7.9994-0.14891s3.9792-1.1913 4.7176-1.1913 0.80066 0.0251 0.80066 0.0251"] []
        , Svg.path [
            d "m89.505 28.066 7.9994 0.1489s3.9791 1.1913 4.7175 1.1913h0.52807"] []
        , Svg.path [
            d "m102.79 15.926 1.4768-4.0205 8.1224 0.29783 0.95686-0.0934 0.0277 5.9009-5.2925 0.007-5.2925 0.007-0.41022-0.002z"] []
        , Svg.path [
            d "m113.37 16.214 13.085 0.15779s1.0312 0.74455 1.0312 4.7651v4.0206"] []]
      , g [
          transform "matrix(0 -.18805 .051804 0 83.041 34.248)"
        , stroke "#000"
        , strokeWidth "20.263"
        ] [
          polygon [
            points "126.17 803.18 126.17 829.41 85.79 825.08 85.333 781.74 85.333 781.74"] []
        , polygon [
            points "126.17 803.18 126.17 829.41 85.79 825.08 85.333 781.74 85.333 781.74"] []]
      , Svg.path [
          d "m113.4 29.091 13.038-0.1134s1.0312-0.74455 1.0312-4.7651v-4.0206"
        , stroke "#000"
        , strokeWidth "2"
        ] []
      , g [
          stroke "#000"
        ] [
          g [
            transform "matrix(0 -.18805 .051804 0 83.03 33.749)"
          , strokeWidth "20.263"] [
            polygon [
              points "0.5 804.18 0.5 830.41 40.877 826.08 41.333 782.74 41.333 782.74"] []
          , polygon [
              points "0.5 804.18 0.5 830.41 40.877 826.08 41.333 782.74 41.333 782.74"] []]
        , g [
            transform "matrix(0 -.20839 .051804 0 83.019 35.973)"
          , strokeWidth "19.249"] [
            rect [
              height "22.357"
            , width "60.276"
            , y "838.52"
            , x "33.579"] []
          , rect [
              height "22.357"
            , width "60.276"
            , y "838.52"
            , x "33.579"] []]
        , g [
            transform "matrix(0 -.20839 .051804 0 83.019 35.973)"
          , strokeWidth "19.249"] [
            rect [
              height "9.267"
            , width "64.866"
            , y "848.08"
            , x "31.196"] []
          , rect [
              height "9.267"
            , width "64.866"
            , y "848.08"
            , x "31.196"] []]]
      , Svg.path [
          d "m102.8 29.515 1.4768 4.0206 8.1224-0.29781 0.95687 0.0934 0.0277-5.9009-5.2925-0.007-5.2925-0.007-0.41021 0.002z"
        , stroke "#000"
        , strokeWidth "2"
        ] []]]]

netRobotsLogo : String -> String -> Html.Html Msg
netRobotsLogo w h =
  Svg.svg [preserveAspectRatio "xMinYMin meet", viewBox "0 0 350 75", width w,  height h] [
   g [
      Svg.Attributes.style "overflow:hidden; text-anchor: middle; font-size:45; font-weight: bold; font-family: Impact"] [
      text' [
        x "175"
      , y "55"
      , Svg.Attributes.style "fill: white; stroke: #0f9; stroke-width: 14"] [Svg.text "NetRobots"]
    , text' [
        x "175"
      , y "55"
      , Svg.Attributes.style "fill: white; stroke: #99f; stroke-width: 8"] [Svg.text "NetRobots"]
    , text' [
        x "175"
      , y "55"
      , Svg.Attributes.style "fill: white; stroke: black; stroke-width: 2"] [Svg.text "NetRobots"]]]

-- -------------------------------------------
-- JSON Decoders

boardInfoDecoder : Decoder BoardInfo
boardInfoDecoder =
    object8 BoardInfo
        ("maxBoardX" := float)
        ("maxBoardY" := float)
        ("streamDelay" := float)
        ("turnDeltaTime" := float)
        ("networkLatency" := float)
        ("startTime" := float)
        ("endTime" := float)
        ("events" := list boardEventDecoder)

robotInfoDecoder : Decoder RobotInfo
robotInfoDecoder =
  let part1 = object6
                (,,,,,) 
                ("robotId" := int)
                ("posX" := float)
                ("posY" := float)
                ("direction" := float)
                ("currentSpeed" := float)
                ("requiredSpeed" := float)
 
      part2 (robotId, posX, posY, direction, currentSpeed, requiredSpeed) =
        object5 (RobotInfo robotId posX posY direction currentSpeed requiredSpeed)
          ("acceleration" := float)
          ("reloadingTime" := float)
          ("health" := float)
          ("points" := float)
          ("missedTurns" := int)

  in part1 `andThen` part2

boardEventDecoder : Decoder BoardEvent
boardEventDecoder =
    object2 BoardEvent
        (("eventType" := int) `andThen` boardEventVariantDecoder)
        ("activationTime" := float)

boardEventVariantDecoder : Int -> Decoder BoardEventVariant
boardEventVariantDecoder eventType =
    case eventType of
        1 -> map ECreateRobot eventCreateRobotDecoder
        2 -> map ERemoveRobot eventRemoveRobotDecoder
        3 -> map EScan eventScanDecoder
        4 -> map EMissile eventMissileDecoder
        5 -> map EExplosion eventExplosionDecoder
        6 -> map ERobotCollision eventRobotCollisionDecoder
        7 -> map EDrive eventDriveDecoder
        _ -> Debug.crash ("Unknown eventType tag " ++ (toString eventType))

jstring : Decoder String
jstring = Json.Decode.string

eventCreateRobotDecoder : Decoder EventCreateRobot
eventCreateRobotDecoder =
  object3 EventCreateRobot
      ("robot" := robotInfoDecoder)
      ("name" := jstring)
      ("color" := jstring)

eventMissileDecoder : Decoder EventMissile
eventMissileDecoder =
  object4 EventMissile
      ("robot" := robotInfoDecoder)
      ("direction" := float)
      ("distance" := float)
      ("speed" := float)

eventScanDecoder : Decoder EventScan
eventScanDecoder =
   object5 EventScan
      ("direction" := float)
      ("semiaperture" := float)
      ("scanMaxDistance" := float)
      ("robot" := robotInfoDecoder)
      (maybe ("hitRobot" := robotInfoDecoder))

eventExplosionDecoder : Decoder EventExplosion
eventExplosionDecoder =
  object3 EventExplosion
      ("robot" := robotInfoDecoder)
      ("hitRobot" := robotInfoDecoder)
      ("damage" := float)

eventRobotCollisionDecoder : Decoder EventRobotCollision
eventRobotCollisionDecoder =
  object1 EventRobotCollision
      ("robot" := robotInfoDecoder)

eventDriveDecoder : Decoder EventDrive
eventDriveDecoder =
  object1 EventDrive
    ("robot" := robotInfoDecoder)

eventRemoveRobotDecoder : Decoder EventRemoveRobot
eventRemoveRobotDecoder =
  object1 EventRemoveRobot
      ("robot" := robotInfoDecoder)

