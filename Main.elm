import Html
import Html.Attributes as HA
url w h u = html w h
                      ( Html.img [HA.width (floor w), HA.height (floor h)
                                 , HA.align "centered"
                                 , HA.src u
                                 ]
                                 []
                      )  
                      

type Msg = Tick Float GetKeyState
         | ToUpperEngine 
         | ToMedBay 
         | ToReactor 
         | ToSecurity 
         | ToLowerEngine 
         | ToElectrical 
         | ToStorage 
         | ToAdmin 
         | ToCafeteria 
         | StartCleanup 
         | FinishCleanup 
         | StartCardSwipe 
         | FinishCardSwipe 
         | StartWire 
         | FinishWire 
         | StartPasscode 
         | FinishPasscode 
         | StartGame 
         
         | CARDMouseDownAt (Float,Float) (Float,Float)
         | CARDMouseMoveTo (Float,Float)
         | CARDStop 
         
         | LEAFMouseDownAt (Leaf) (Float,Float) 
         | LEAFMouseMoveTo (Float,Float)
         | LEAFStop 
         
         | WIREMouseDownAt (Float,Float) (Float,Float)
         | WIREMouseMoveTo (Float,Float)
         | WIREStop
         
         | PASSClick Int 

type LocationState = Cafeteria 
           | UpperEngine 
           | MedBay 
           | Reactor 
           | Security 
           | LowerEngine 
           | Electrical 
           | Storage 
           | Admin 
           
           | CleanupTask 
           | CardSwipeTask 
           | WireTask 
           | PasscodeTask 
           | Start 

           
           
type State =    Empty
              | CARDWaiting
              | CARDGrabbed 
                  (Float,Float) -- offset from down position to draw position
                  (Float,Float) -- current mouse position
              | CARDWin
              
              --
              
              | LEAFWaiting
              | LEAFWin
              | LEAFLose
              | LEAFGrabbed 
                  (Float,Float) -- offset from down position to draw position
                  (Float,Float) -- current mouse position
                  (Float) -- Id
                  (Float) -- Rotation
                  
                        
             | WIREWaiting
             | WiresDone
             | WIREGrabbed 
               (Float,Float) -- offset from down position to draw position
               (Float,Float) -- current mouse position
               
             | PASSDisplaying Float Int 
             | PASSWaiting
             | PASSRightPick Float Int 
             | PASSWrongPick Float 
             | PASSFinished
             | PASSPreDisplay Float

update msg model =
    case msg of
        Tick t _ ->
            
            case model.overworldState of
                CleanupTask ->  
                    if model.leavesLeft <= 0 then
                       { model | time = t,
                                 state = LEAFWin }
                      else
                      if model.timer > 0 then
                        { model | 
                            time = t,
                            timer = (model.timer - (1/60)),
                            points = if model.stop == False then mutatePoses model.points model else model.points,
                            stop = False
                            }
                      else
                        { model | time = t,
                                 state = LEAFLose }
                
                
                PasscodeTask ->
                    let lastTime = if model.time > 0 then model.time else t
                    in case model.state of 
                      PASSDisplaying timeLeft round ->
                        let 
                          newModel = if timeLeft < t - lastTime && round == 0  then 
                                       {model | time = t, state = PASSWaiting}
                                     else if timeLeft < t - lastTime then
                                       {model | time = t, state = PASSDisplaying 0.5 (round - 1)}
                                     else 
                                       {model | time = t, state = PASSDisplaying (timeLeft - (t-lastTime)) round} 
                        in
                          newModel
                      PASSWaiting ->
                        {model | time = t }
                      PASSRightPick timeLeft pick -> 
                        let    
                          newModel = if timeLeft < t - lastTime && model.round == 4 && model.counter == 4 then
                                       {model | time = t
                                              , state = PASSFinished
                                              }
                                     else if timeLeft < t - lastTime && model.counter == model.round then 
                                      {model | time = t
                                             , state = PASSDisplaying 0.5 ( (model.round + 1))
                                             , round = model.round + 1
                                             , counter = 0
                                             , lights = 0
                                             }
                                     else if timeLeft < t - lastTime then 
                                       {model | time = t, state = PASSWaiting, counter = model.counter + 1}
                                     else
                                       {model | time = t, state = PASSRightPick (timeLeft - (t - lastTime)) pick}          

              --            newState = if model.counter == 5 then
                        in newModel   
                      PASSWrongPick timeLeft->
                        if timeLeft < (t - lastTime) then
                          {model|time = t, state = PASSDisplaying 0.5 0, round = 0, counter = 0, lights = 0}
                        else
                          {model|time=t, state = PASSWrongPick (timeLeft - (t - lastTime))}
                      _ -> model
                _ -> {model | time = t}
                  
                  
            
            
            
            
            
            
            
         --This is so much more readable.
         
         --The option to go to some other room is possible; but will never happen.
         --a) there's no notify for those transitions if it's not possible to get to the room
         --b) pure functions! 
         --c) this looks far better.
         
        ToUpperEngine  -> { model | overworldState = UpperEngine  }
        ToMedBay  -> { model | overworldState = MedBay  }
        ToReactor  ->  { model | overworldState = Reactor  }
        ToSecurity  -> { model | overworldState = Security  }
        ToLowerEngine  -> { model | overworldState = LowerEngine }
        ToElectrical  -> { model | overworldState = Electrical  }
        ToStorage  ->  { model | overworldState = Storage  }
        ToAdmin  -> { model | overworldState = Admin  }
        ToCafeteria  ->  { model | overworldState = Cafeteria  }
        
        
        StartCleanup  ->  { model | overworldState = CleanupTask, state = LEAFWaiting
                                                                   ,leavesLeft = leavesToSpawn
                                                                   ,timer = leavesTimerInitTime
                                                                   ,points = orgPosses  }
        FinishCleanup  ->  { model | overworldState = Reactor, state = Empty  }
           
        StartCardSwipe  -> { model | overworldState = CardSwipeTask, state = CARDWaiting   }
        FinishCardSwipe  -> { model | overworldState = Admin, state = Empty  }

        StartWire  -> { model | overworldState = WireTask, state = WIREWaiting, wire_points = initialPosition  }
        FinishWire  -> { model | overworldState = Electrical, state = Empty  }
        
        StartPasscode  -> { model | overworldState = PasscodeTask, state = PASSDisplaying 1 0  }
        FinishPasscode  -> { model | overworldState = Security, state = Empty  }

        StartGame  -> { model | overworldState = Cafeteria  }

        --
        -- Messages For Each MINI GAME!! Don't want to mix them
        -- As they might have different functions.
        -- Plus, I'd end up making a case statement within this anyways
        -- So this is actually less complicated.
        --
        CARDMouseDownAt orig mouseAt ->
            { model | state = CARDGrabbed (sub orig mouseAt) mouseAt,
                     cardGoodRead = True,
                     cardWasInBox = False,
                     cardTimeEnterBox = -100,
                     cardTimeExitBox = 0,
                     cardInBox = False,
                     cardErrorMessage = ""} --reset good read

        CARDMouseMoveTo new ->
            case model.state of 
              CARDGrabbed delta _ ->
                let (newX, newY) = (cardPosClamp new) in


                { model |
                          cardVel = (sub (new) (model.cardLastPos)),
                          cardLastPos = new,

                          --Logic
                          cardTimeEnterBox = card_logicTimeEnter model (newX, newY),
                          cardTimeExitBox = card_logicTimeExit model (newX, newY),

                          cardInBox = cardInsideReader (newX, newY+1),
                          cardWasInBox = if model.cardInBox then True else model.cardWasInBox,
                          cardGoodRead = card_logicFail model (newX, newY),

                          cardCompleted = card_logicWin model,

                          state = CARDGrabbed delta (newX, newY)

                          }
              _ -> 
                model

        CARDStop -> 
            case model.state of 
              CARDGrabbed delta mouseAt ->
                { model | state = if model.cardCompleted then CARDWin else CARDWaiting,
                          cardErrorMessage = (if model.cardCompleted then "Good Read" else
                                                if (model.cardTimeEnterBox == -100) then
                                                  model.cardErrorMessage
                                                else
                                                if (model.cardTimeExitBox == 0) then
                                                  "Bad Read"
                                                else
                                                  if (model.cardTimeExitBox - model.cardTimeEnterBox) < 1 then "Too Fast" else
                                                  if (model.cardTimeExitBox - model.cardTimeEnterBox) > 2 then "Too Slow" else
                                                  "Bad Read"
                                               )

                                                }
              _ -> 
                model  
                
       --Leaf Messages 
        LEAFMouseDownAt orig mouseAt ->
          { model | 
                   state = LEAFGrabbed (sub (getLeafPos orig) mouseAt) mouseAt (getLeafId orig) (getLeafRot orig)
                  , points = List.filter ( \ (Leaf pos id r) -> id /= (getLeafId orig) ) model.points
                  , stop = True
                  }
                
        LEAFMouseMoveTo new ->
          case model.state of 
            LEAFGrabbed delta _ id rot ->
              { model | state = LEAFGrabbed delta new id rot}
            _ -> 
              model
        LEAFStop -> 
          case model.state of 
            LEAFGrabbed delta mouseAt grabbedId grabbedRotation ->

              if (inside_vaccum (add delta mouseAt)) then
                { model | state = LEAFWaiting
                        , points = model.points
                        , leavesLeft = model.leavesLeft - 1}
              else 
                { model | state = LEAFWaiting
                        , points = (Leaf (add delta mouseAt) grabbedId grabbedRotation) :: model.points }
            _ -> model
            
        WIREMouseDownAt orig mouseAt ->
          { model | wire_points = filter model.wire_points orig
                  , state = WIREGrabbed (sub orig mouseAt) mouseAt
                  }
        WIREMouseMoveTo new ->
          case model.state of 
            WIREGrabbed delta _ ->
              { model | state = WIREGrabbed delta new }
            _ -> 
              model
        WIREStop -> 
          case model.state of 
            WIREGrabbed delta mouseAt ->
              { model | state = if (checkConnect model.wire_points (add delta mouseAt) finalPosition initialPosition) /= finalPosition then WIREWaiting else WiresDone
                      , wire_points = checkConnect model.wire_points (add delta mouseAt) finalPosition initialPosition}
            _ -> 
              model
              
        PASSClick pick -> 
                    case model.state of 
                      PASSWaiting -> 
                        if pick == listPick (model.counter) model.pattern then
                          {model | state = PASSRightPick 0.1 pick, lights = model.lights + 1}
                        else 
                          {model | state = PASSWrongPick 1, lights = 5}
                      _ -> model


--
--
-- END UPDATE MSG!!
--
  
type alias Model =
    { time : Float,
      overworldState : LocationState,
      state : State,

    --
    --Card Attributes
    --
    cardLastPos : (Float, Float),
    cardVel : (Float, Float),
    cardTimeEnterBox : Float,
    cardTimeExitBox : Float,
    cardInBox : Bool,
    cardWasInBox : Bool,
    cardGoodRead : Bool,
    cardCompleted : Bool,
    cardErrorMessage : String,
    
    --
    -- Leaf Attributes
    --
    timer : Float,
    leavesLeft : Int,
    stop : Bool,
    points : List Leaf,
    
    --WiRE
    wire_points : List (Float, Float)
    
    --PASS
    ,round : Int
    ,counter : Int
    ,pattern : List Int
    ,lights : Int
    
    }

type alias Point = (Float, Float)

init : Model
init = { time = 0 ,
        state = Empty ,
        overworldState = Start ,
       
       --CARD
        cardLastPos = cardPos, 
        cardVel = (0, 0),
        cardTimeEnterBox = 0,
        cardTimeExitBox = 0,
        cardInBox = False,
        cardGoodRead = False,
        cardCompleted = False,
        cardWasInBox = False,
        cardErrorMessage = "",
        
        --LEAF
        timer = leavesTimerInitTime,
        leavesLeft = leavesToSpawn,
        stop = False,
        points = orgPosses,
        
        --WIRE
        wire_points = initialPosition,
        
        --Pass
        round = 0,
        counter = 0,
        pattern = [3,2,8,7,9],
        lights = 0
       }
    
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}


myShapes model =
    case model.overworldState of
        Cafeteria  ->
            [ cafeteriaCheckpoint True (60, 0) ]
        MedBay  ->
            [ cafeteriaCheckpoint False (0, -30) ]
            
        UpperEngine  ->
            [uperEngineCheckpoint True (10, 40)]        
        Reactor  ->
            [uperEngineCheckpoint False (-20, -10),
            taskAlert |> move (-20, 20) |> notifyTap StartCleanup]
        Security  ->
            [uperEngineCheckpoint False (30, -10),
            taskAlert |> move (30, 15) |> notifyTap StartPasscode]
            
            
        LowerEngine  ->
            [lowerEngineCheckpoint True (-40, 0)]     
        Electrical  ->
            [lowerEngineCheckpoint False (12, 5),
            taskAlert |> move (25, 20) |> notifyTap StartWire]
        
        
        Storage  ->
            [storageCheckpoint True (10, -40)]   
            
        Admin  ->
            [storageCheckpoint False (45, -15),
            taskAlert |> move (50, -30) |> notifyTap StartCardSwipe]
            
        CleanupTask  ->
            leafMyShape model
        CardSwipeTask  ->
              cardScanner model
        WireTask  ->
            wireShapeModel model
        PasscodeTask  ->
            passwordShapes model
        Start  ->
            [
            titleModel ((model.time) / 10)
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "Start Game"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap StartGame
            ]
            
            



{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--          CARD SWIPE!!!!!!!!!!!!!!!!!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}
 
 
cardUrl = url 50 30 "https://cdn.discordapp.com/attachments/912226502827966474/952752676410646538/unknown.png" 
        |> move (-25, 13)

walletBkg = url 120 52 "https://cdn.discordapp.com/attachments/912226502827966474/952752783394758746/unknown.png" 
scannerTop = url 120 50 "https://cdn.discordapp.com/attachments/912226502827966474/952752837153128448/unknown.png"
 
cardAssetsbehindCard model = 
  
  let (w, h) = ((cardReaderXR-cardReaderXL), (cardReaderYT-cardReaderYB)) in
  
  group 
  [
     rect 192 128
      |> filled (rgb 10 10 10)
     , rect w h
       |> filled red
       |> move (cardReaderXL+w/2, cardReaderYB+h/2)
     , walletBkg |> move (-50, -20)
  ]
  
  
cardAssetsinfrontCard model = group 
  [
    scannerTop |> move (-60, 65),
    text ( model.cardErrorMessage) |> filled white |> scale 0.75 |> move (-48, 53) 
  ]

--THIS IS THE MYSHAPES OF THE CARD TASK!!!
cardScanner model = 
  (
  case model.state of
    CARDWaiting ->
      [
      cardAssetsbehindCard model,
      cardUrl
        |> move cardPos      
      ,card 
        |> move cardPos 
        |> makeTransparent 0
        |> notifyMouseDownAt (CARDMouseDownAt cardPos)
      ,cardAssetsinfrontCard model
      
      ]
    CARDGrabbed delta mouseAt ->
      [ 
       cardAssetsbehindCard model
       , card
          |> move (add delta mouseAt)
       , cardUrl
        |> move (add delta mouseAt)
       ,cardAssetsinfrontCard model

      , rect 190 126 |> filled (rgba 255 255 0 0.01)
          |> notifyMouseUp CARDStop
          |> notifyLeave CARDStop
          |> notifyMouseMoveAt CARDMouseMoveTo
      ] 
    CARDWin ->
      [
      
      cardAssetsbehindCard model,
      cardUrl
        |> move cardPos
      ,cardAssetsinfrontCard model
      ,
        group
                  [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "Complete Task"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]|> notifyTap FinishCardSwipe
      ]
    --Nothing If Else
    _ -> []
   )
    
      

vectorToStr (x, y) = "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
boolToStr bool = if bool then "True" else "False"
cardPos = (-20, -45)

card_logicTimeEnter model (x, y) = if cardInsideReader (x, y+2) then
                              if x < cardReaderXL+10 then
                                if model.cardInBox then 
                                  --Inside Before
                                  model.cardTimeEnterBox
                                else
                                  --Just Entered
                                  model.time
                               else
                                 --Not Inside, Do No Changes
                                 model.cardTimeEnterBox
                             else
                               model.cardTimeEnterBox
                               
card_logicTimeExit model (x, y) = if cardInsideReader (x, y+2) then
                                 --Not Outside, Do No Changes
                                 model.cardTimeExitBox
                             else
                               if model.cardInBox then
                                --Just Exited
                                if model.cardWasInBox then
                                  model.time
                                else
                                  model.cardTimeExitBox
                              else
                                --Exited Prev; No Changes
                                model.cardTimeExitBox
                 
--Logic... See if Card Goes Proper Direction
card_logicFail model (x, y) = let (hs, vs) = model.cardVel in
                          
                          if model.cardWasInBox then
                            if model.cardInBox then

                                if hs < 0 then
                                  False
                                else
                                  model.cardGoodRead

                            else
                              --See Exit
                              if x <= cardReaderXR then
                                False
                              else
                                model.cardGoodRead
                           else
                             True

card_logicWin model = let time = (model.cardTimeExitBox - model.cardTimeEnterBox) in
                          1 <= time && time <= 2 && model.cardGoodRead

sub (x,y) (u,v) = (x-u,y-v)
add (x,y) (u,v) = (x+u,y+v)


card = group
  [
    roundedRect 50 25 3
      |> filled white
      |> addOutline (solid 1) (black)
  ]

cardReaderXL = -50
cardReaderXR = 50
cardReaderYT = 70
cardReaderYB = 15

cardPosClamp (x,y) = (x, min y cardReaderYB)

cardInsideReader (x, y) = (cardReaderXL < x && x < cardReaderXR) && (cardReaderYB < y && y < cardReaderYT)

cardScanMachine = group 
  [
   rect 200 150
      |>filled black
  , roundedRect 120 50 10
      |> filled (rgb 62 64 71)
      |> move(0,45)
      |> addOutline (solid 2) (black)
  , roundedRect 120 100 10
      |> filled (rgb 62 64 71)
      |> move(0,-35)
      |> addOutline (solid 2) (black)
  , rect 120 70
      |> filled (rgb 105 110 104)
      |> move(0,-35)
      |> addOutline (solid 2) (black)
  , rect 100 15
      |> filled (rgb 25 188 99)
      |> move(0,50)
      |> addOutline (solid 2) (black)
  , circle 4
      |> filled (rgb 255 31 31)
      |> addOutline (solid 1) (black)
      |> move (35,30)
  , circle 4
      |> filled (rgb 89 237 81)
      |> addOutline (solid 1) (black)
      |> move (45,30)
  ]

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--         LEAF / CLEAN UP!!!!!!!!!!!!!!!!!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

type Leaf = Leaf (Float, Float) Float Float

leavesGoalX = -75
leavesGoalW = 17
leavesGoalY = -6
leavesGoalH = 78

leafHitboxW = 11
leafHitboxH = 33

inside_vaccum (x, y) = 
  x > leavesGoalX - leavesGoalW/2 &&
  x < leavesGoalX + leavesGoalW/2 &&
  y > leavesGoalY - leavesGoalH/2 &&
  y < leavesGoalY + leavesGoalH/2
                
amongus = url 15 19 "https://borderpolar.com/wp-content/uploads/2021/07/Banana.png"
leafbackground = url 205 768 "https://github.com/Overload02/among-us-assets/blob/main/Tasks/EmptyGarbage-sharedassets0.assets-63.png?raw=true"
                     |> move (-100, 280)

leafimg0 = url 58 160 "https://cdn.discordapp.com/attachments/908256031509979156/950410097534832670/unknown.png" |> scale 0.2 |> move (-5, 15)
leavesToSpawn = 5
leavesTimerInitTime = 31.0

leaves model = 
  case model.state of
    LEAFWaiting ->
      model.points
        |> List.map 
            ( \ (Leaf pos id rot) -> group [    
                        leafimg0
                           |> rotate rot
                           |> move pos,
                        rect leafHitboxW leafHitboxH |> filled red
                          |> rotate rot
                          |> move pos
                          |> makeTransparent 0
                          |> notifyMouseDownAt (LEAFMouseDownAt (Leaf pos id rot))
                      ]
                 
            )
    LEAFGrabbed delta mouseAt idd grabbedAngle ->
      ( model.points
        |> List.map 
            ( \ (Leaf pos id rot) -> group [    
                        leafimg0
                            |> rotate rot
                           |> move pos
                           
                      ]
            )
      ) 
      ++
      [leafimg0
          |> rotate grabbedAngle
         |> move (add delta mouseAt)
      , rect 190 126 |> filled (rgba 255 255 0 0.01)
          |> notifyMouseUp LEAFStop
          |> notifyLeave LEAFStop
          |> notifyMouseMoveAt LEAFMouseMoveTo
      ]
    _ -> loseGameState

otherElements model = 
  [
    text ("Time Left: " ++ String.fromInt (floor model.timer))
      |> filled black
      |> scale 0.5
      |> move (-95, 53)
  ,
    text ("Leaves Left: " ++ String.fromInt model.leavesLeft)
      |> filled black
      |> scale 0.5
      |> move (-95, 46)
  --, amongus
  ]

              --Ideally, the text would be infront of the leaves, but drag and drop causes
              --weird behaviour. This is a neccesary sacrifice.
leavesGameState model = [leafbackground] ++ (otherElements model) ++ leaves model
winGameState = [leafbackground, group
                  [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "Complete Task"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]|> notifyTap FinishCleanup]
loseGameState = [leafbackground, group
                  [
                       roundedRect 60 20 5
                            |> filled red
                  ,    text "Try Again"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]|> notifyTap StartCleanup]

leafMyShape model = 
    case model.state of
        LEAFWin -> winGameState
        LEAFLose -> loseGameState
        _ -> leavesGameState model 
    
  
getLeafPos (Leaf pos _ _) = pos
getLeafId (Leaf _ id _) = id
getLeafRot (Leaf _ _ rot) = rot

orgPosses = [
   Leaf (22,13) 0 0,
   Leaf (47,-10) 1 270,
   Leaf (-20,5) 2 123,
   Leaf (60,25) 3 49,
   Leaf (20,-34) 4 194
  ]

mutatePoses list model =  
  case list of
    (Leaf (x, y) id r)::xs -> [Leaf (mutatePos (x, y) model.time id 0.2) id (mutationRotation r model.time id)] ++ (mutatePoses xs model)
    [] -> []

mutatePos (x, y) t i s = (x + (cos (t + i*(-245))*s), y + (sin (t + i*1234))*s)
mutationRotation r t i = r + sin(t/400 + (i+1)*457)/100




{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--         WIRES!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

cbPink =    (rgb 0xF0 0x54 0x54)
cbBlue =    (rgb 0x64 0x8f 0xff)
cbOrange  = (rgb 0xfe 0x61 0x00)
cbYellow =  (rgb 0xff 0xb0 0x00)

clrL = [cbPink,cbYellow,cbOrange,cbBlue]

circFinal clr pos =
  circle 3
    |> filled clr
    |> move pos
wireEnd clr pos =
  rect 6 6
    |> filled clr
    |> move (case pos of
            (x,y) -> ((abs x + 2.1) * x/(abs x),y))

wireShapeModel model =
  case model.state of
    WiresDone ->
        [
        roundedRect 80 80 5
          |> filled black
        , (background
          |> move (-37.5,37.5))
        ]
        ++
        List.map2 wireEnd clrL finalPosition
        ++
        List.map2 wireEnd clrL initialPosition
        ++
        List.map2 circFinal clrL finalPosition
        ++
        List.map2 circFinal clrL initialPosition
        ++
        (List.map3 wire clrL initialPosition model.wire_points)
        ++
        ( model.wire_points
          |> List.map 
              ( \ pos -> circle 3 |> filled gray
                        |> move pos
              ))
        ++
        [group [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "Complete Task"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ] |> notifyTap FinishWire
                  ]
    
    WIREWaiting ->
        [
        roundedRect 80 80 5
          |> filled black
        , (background
          |> move (-37.5,37.5))
        ]
        ++
        List.map2 wireEnd clrL finalPosition
        ++
        List.map2 wireEnd clrL initialPosition
        ++
        List.map2 circFinal clrL finalPosition
        ++
        List.map2 circFinal clrL initialPosition
        ++
        (List.map3 wire clrL initialPosition model.wire_points)
        ++
        ( model.wire_points
          |> List.map 
              ( \ pos -> circle 3 |> filled gray
                        |> move pos
                        |> notifyMouseDownAt  (WIREMouseDownAt pos)
              ))
                        
    WIREGrabbed delta mouseAt ->
        [
          roundedRect 80 80 5
            |> filled black
        , (background
            |> move (-37.5,37.5))
        ]
        ++
        List.map2 wireEnd clrL finalPosition
        ++
        List.map2 wireEnd clrL initialPosition
        ++
        List.map2 circFinal clrL finalPosition
        ++
        List.map2 circFinal clrL initialPosition
        ++
        (List.map3 wire clrL initialPosition (insert model.wire_points (add delta mouseAt)))
        ++
        ( model.wire_points
          |> List.map 
            ( \ pos -> circle 3 
                        |> filled gray
                        |> move pos
            )
        )
        ++ 
        [ circle 3 |> filled gray |> addOutline (solid 1) orange
            |> move (add delta mouseAt)
        , rect 190 126 |> filled (rgba 255 255 0 0.01)
            |> notifyMouseUp WIREStop
            |> notifyLeave WIREStop
            |> notifyMouseMoveAt WIREMouseMoveTo
        ]
        
    _ -> []
      

error = 3.0

check (x,y) (xc,yc) = 
      if abs (x-xc) < error && abs (y-yc) < error then True else False

wire clr initial current =
  line initial current
    |> outlined (solid 6) clr

filter list orig = case list of
  [] -> []
  (x::xs) -> if x == orig then (-1000,-1000) :: filter xs orig else x :: filter xs orig 

insert list point = case list of
  [] -> []
  (x::xs) -> if x == (-1000,-1000) then point::xs else x :: insert xs point

checkConnect list point listF listI = case (list,listF,listI) of
  (x::xs,f::fs,i::is) -> if x /= (-1000,-1000) then x :: (checkConnect xs point fs is) else if check point f then f :: xs else i :: xs
  (_,_,_) -> []
        
--points = initialPosition

initialPosition = 
  [(-32,23.25)
  ,(-32,7.75)
  ,(-32,-7.75)
  ,(-32,-23.25)
  ]
finalPosition =
  [(32,-23.25)
  ,(32,-7.75)
  ,(32, 23.25)
  ,(32, 7.75)
  ]

background = url 75 75 "https://static.wikia.nocookie.net/among-us-wiki/images/1/1e/Fix_Wiring_Backdrop.png"
samosa = url 75 75 "https://crescentfood.com/wp-content/uploads/2020/05/samosa-1.png"


{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--         WIRES!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

passwordShapes model = 
  [
  rightPad model,
  leftPad model
-- debug 
{-
  ,text ("round: " ++ String.fromInt model.round)
    |> filled black
    |> move (-50, 50)
  ,
  text ("counter: " ++ String.fromInt model.counter)
    |> filled black
    |> move (0, 50)
  ,

  let 
    s = case model.state of
          PASSDisplaying a b -> "Displaying:" ++ (String.fromFloat a)
          PASSRightPick _ _ -> "Right"
          PASSWrongPick _ -> "Wrong"
          PASSWaiting -> "Wait"
          PASSFinished -> "Fini"
          _ -> ""
  in
    text ("state: " ++ s)
      |> filled black
      |> move (0, -50)
-}
  ] ++ 
  if model.state == PASSFinished then
               [group
                  [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "Complete Task"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]|> notifyTap FinishPasscode
                  ]
           else 
           []

rightPad model = group
  [
    rect 70 80
      |> filled lightCharcoal
      |> addOutline (solid 1) black
      |> move (40,0)
    ,
   let 
     colour = case model.state of
               PASSWaiting -> grey
               PASSRightPick _ _-> grey 
               PASSFinished -> blue
               PASSWrongPick _ -> red
               _ -> charcoal
     -- row 3
   in
     group [
     square 17 
        |> filled colour
        |> addOutline (solid 1) black
        |> move (60, -25)
        |> notifyTap (PASSClick 9)
     ,
       square 17 
        |> filled colour
        |> addOutline (solid 1) black
        |> move (40, -25)
        |> notifyTap (PASSClick 8)
     ,
       square 17 
        |> filled colour
        |> addOutline (solid 1) black
        |> move (20, -25)
        |> notifyTap (PASSClick 7)
     ,
     -- row 2
        square 17 
        |> filled colour
        |> addOutline (solid 1) black
        |> move (60, -5)
        |> notifyTap (PASSClick 6)
     ,
       square 17 
        |> filled colour
        |> addOutline (solid 1) black
        |> move (40, -5)
        |> notifyTap (PASSClick 5)
     ,
       square 17
        |> filled colour
        |> addOutline (solid 1) black
        |> move (20, -5)
        |> notifyTap (PASSClick 4)
     ,
     -- row 1
      square 17
        |> filled colour
        |> addOutline (solid 1) black
        |> move (60, 15)
        |> notifyTap (PASSClick 3)
      ,
     square 17
        |> filled colour
        |> addOutline (solid 1) black
        |> move (40, 15)
        |> notifyTap (PASSClick 2)
     ,
     square 17
        |> filled colour
        |> addOutline (solid 1) black
        |> move (20, 15)
        |> notifyTap (PASSClick 1)
    ]
    ,
   let 
     colour = case model.state of 
               PASSRightPick _ _ -> blue
               _ -> blank
     pos = case model.state of 
             PASSRightPick _ n -> 
               case n of 
                 1 -> (20, 15)
                 2 -> (40, 15)
                 3 -> (60, 15)
                 4 -> (20, -5)
                 5 -> (40, -5)
                 6 -> (60, -5)
                 7 -> (20, -25)
                 8 -> (40, -25)
                 9 -> (60, -25)
                 _ -> (0,0)  
             _ -> (0,0)
   in 
     square 17
       |> filled colour
       |> move pos
   ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (20, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (30, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (40, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (50, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (60, 33)
   ,
   let 
     colour = case model.state of
               PASSWrongPick _ -> red
               _ -> green
   in
     List.map (\x -> circle 3 |> filled colour |> move (20 + 10 * toFloat x, 33) ) (List.range 0 (model.lights-1))
       |> group    
  ]


leftPad model = group
  [
   -- background
   rect 70 80
     |> filled lightCharcoal
     |> addOutline (solid 1) black
     |> move (-40,0)
    ,  
    -- black background
   square 60 
      |> filled black
      |> addOutline (solid 0.5) white
      |> move (-40, -5)
   ,
   let 
     colour = case model.state of 
               PASSDisplaying _ _ -> blue
               _ -> blank
     pos = case model.state of 
             PASSDisplaying _ n -> 
               case (listPick (model.round - n) model.pattern) of 
                 1 -> (-60, 15)
                 2 -> (-40, 15)
                 3 -> (-20, 15)
                 4 -> (-60, -5)
                 5 -> (-40, -5)
                 6 -> (-20, -5)
                 7 -> (-60, -25)
                 8 -> (-40, -25)
                 9 -> (-20, -25)
                 _ -> (0,0)  
             _ -> (0,0)
   in 
     square 17
       |> filled colour
       |> move pos
   ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (-20, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (-30, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (-40, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (-50, 33)
          ,
     circle 3
       |> filled charcoal
       |> addOutline (solid 1) black
       |> move (-60, 33)
    
   ,
   List.map (\x -> circle 3 |> filled green |> move (-60 + 10* toFloat x, 33) )  (List.range 0 model.round)
     |> group
     |> addOutline (solid 1) black
  ]
  
  

--Button Creator & Reposition
button str msgToSend coord col = buttonBase str
  |> scale 0.75
  |> move coord
  |> notifyTap msgToSend 

--Button Group Model
buttonBase str = group
  [
        roundedRect 40 20 5
          |> filled green
        ,text str
          |> centered
          |> size 8
          |> filled black
          |> move(0, -3)
  ]

listPick i lst = case (i,lst) of
                   (0, x :: _) -> x
                   (1, _ :: x :: _ ) -> x
                   (2, _ :: _ :: x :: _ ) -> x
                   (3, _ :: _ :: _ :: x :: _) -> x
                   (4, _ :: _ :: _ :: _ :: x  :: _) -> x                   
                   (_, _) -> -1


{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--         OVERWORLD!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

crewmate = url 35 30 "https://cdn.discordapp.com/attachments/935183933484707853/952920572894019614/unknown.png" |> move (-20, 20)

mapBackground bkgObj useCheckpoint = group [rect 192 128 |> filled black, map bkgObj useCheckpoint]

map bkgObj useAt = (if useAt then
                         url 192 128 bkgObj.linkAtCP
                   else
                         url 192 128 bkgObj.linkNotAtCP
                    ) |> move (-85, 60) |> scale 1.4

type alias Background = {
  linkAtCP : String.String,
  linkNotAtCP : String.String
  }
transitionButton w h message label =
  group [
    rect w h |> filled red |> makeTransparent 0
    , text label |> filled white |> move (-(w + toFloat (String.length label)*2.5)/2, 0) |> scale 0.4
    ] |> notifyTap message



cafCPBkg : Background
cafCPBkg = { 
  linkAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/952917188094877737/unknown.png",
  linkNotAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953650026859622430/unknown.png"
  }
cafeteriaCheckpoint atCp crewmatePos = group [
  (mapBackground cafCPBkg atCp)
  ,transitionButton 33 40 ToUpperEngine "Upper Engine" |> move (-54, 0) 
  ,transitionButton 42 43 ToMedBay "Medbay" |> move (4, -30) 
  , crewmate |> move crewmatePos
  ]
  
  
upEngBkg : Background
upEngBkg = { 
  linkAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/952917215206846524/unknown.png",
  linkNotAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953654506900586506/unknown.png"
  }
uperEngineCheckpoint atCp crewmatePos = group [
  (mapBackground upEngBkg atCp)
  ,transitionButton 33 60 ToReactor "Reactor" |> move (-23.5, -5) 
  ,transitionButton 30 35 ToLowerEngine "Lower Engine" |> move (2, -46) 
  ,transitionButton 20 40 ToSecurity "Security" |> move (28.5, 0) 
  , crewmate |> move crewmatePos
  ]
  
  
lowerEngBkg : Background
lowerEngBkg = { 
  linkAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953656704304816128/unknown.png",
  linkNotAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953656869384253450/unknown.png"
  }
lowerEngineCheckpoint atCp crewmatePos = group [
  (mapBackground lowerEngBkg atCp)
  ,transitionButton 37 42 ToElectrical "Electrical" |> move (15, 10) 
  ,transitionButton 40 70 ToStorage "Storage" |> move (53, -15) 
  , crewmate |> move crewmatePos
  ]
  
storageCheckBkg : Background
storageCheckBkg = { 
  linkAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953658598246973440/unknown.png",
  linkNotAtCP = "https://cdn.discordapp.com/attachments/935183933484707853/953658637014945812/unknown.png"
  }
storageCheckpoint atCp crewmatePos = group [
  (mapBackground storageCheckBkg atCp)
  ,transitionButton 34 38 ToAdmin "Admin" |> move (44, -22) 
  ,transitionButton 75 85 ToCafeteria "Cafeteria" |> move (10, 43) 
  , crewmate |> move crewmatePos
  ]
  
taskAlert = group [

  circle 5 |> filled (rgb 255 224 0),
  circle 5 |> outlined (solid 1) orange,
  
  text "!" |> filled orange |> scale 0.7 |> move (-1.5, -2.5)

  ]
  
{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}

--         TITLE SCREEN!!!!

{-
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
-}


titleModel (t) = group [

    rect 192 128 |> filled black
    , spacebkg
    
    , let (xx, yy, r) = (((sin t) + (sin (4*t))), (cos t)+ (sin (t) * cos (t)), cos(t/3)*18) in
        crewmate |> move (xx*28, yy*16) |> rotate r
      
    , text "Among Us" |> filled white |> scale 1.5 |> move (-35, 20)

  ]
  
  
spacebkg = group [
  stars 23 30 158,
  stars 66 (-54) 19,
  stars 40 37 138,
  stars 63 37 114,
  stars 35 50 14,
  stars 19 (-53) 2,
  stars 72 54 144,
  stars (-21) 24 97,
  stars (-30) (-34) 115,
  stars 79 34 106,
  stars 28 16 181,
  stars 81 60 151,
  stars 30 (-7) 199,
  stars (-63) (-1) 38,
  stars 75 40 58,
  stars 88 57 78,
  stars (-44) (-55) 41,
  stars (-82) (-62) 29,
  stars (-33) 53 115,
  stars 20 32 52,
  stars (-76) 45 0,
  stars (-69) (-54) 97,
  stars 67 (-8) 196,
  stars (-39) 21 58,
  stars 58 39 70,
  stars (-56) 35 167,
  stars (-89) (-13) 97,
  stars 3 (-57) 84,
  stars (-58) (-47) 199,
  stars 64 40 91,
  stars 35 (-16) 134,
  stars (-88) 17 192,
  stars 77 4 11,
  stars (-5) (-21) 159,
  stars (-23) (-57) 54,
  stars (-66) (-22) 11,
  stars 51 (-61) 31,
  stars (-47) 20 122,
  stars 51 58 134,
  stars (-95) (-9) 174,
  stars (-12) (-33) 131,
  stars (-81) 28 177,
  stars 49 (-11) 7,
  stars 54 16 202,
  stars 62 (-4) 204,
  stars (-35) (-16) 33,
  stars 33 63 172,
  stars 47 25 53,
  stars 33 40 56,
  stars (-74) 15 38,
  stars (-35) (-20) 56,
  stars (-56) (-43) 107
  ]
  
stars x y z =
  circle 0.5 
  |> filled (rgb z z z)
  |> rotate (degrees 30)
  |> move (x, y)
