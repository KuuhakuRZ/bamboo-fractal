module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Dict exposing (Dict)

navy = rgb 0 0 200

myShapes : Model -> List (Shape Msg)
myShapes model =
  [ -- show the fractal
    let
      -- generation integer and fractional part
      generation = state2num model.state
      -- this is the "generation" number
      whole = floor generation
      frac = generation - (toFloat whole)
    in
      iterate whole applyRules start
          |> turtleDraw model
          |> group
          |> scale (1^(toFloat whole - 3))
          |> move (-52,-215)
          |> scale 0.5
          |> rotate (degrees -45)
  , case model.state of
      Dragging x -> circle 5 |> filled navy |> move (toPos x, -58)
      Waiting x -> circle 4 |> filled navy |> move (toPos x, -58)
      Animating x -> circle 4 |> outlined (solid 0.5) navy |> move (toPos x, -58)
  , List.map ( \ idx -> String.fromInt idx |> text |> centered |> size 4 |> filled (rgb 100 100 200) |> move (toPos (toFloat idx),-59.5) )
      (List.range 0 5)
      |> group
  , roundedRect 65 10 5 |> filled (rgba 255 0 0 0.5)
      |> move (-25,-58)
      |> ( case model.state of 
             Waiting _  -> notifyMouseDownAt StartDragAt
             Dragging _ -> notifyMouseMoveAt MoveDragAt
                             >> notifyMouseUp StopDrag
                             >> notifyLeave StopDrag
             Animating _ -> identity
         )
  , case model.state of 
      Animating _ -> 
        square 8 |> filled (rgb 100 0 200) |> move (20,-58)
          |> notifyTap StopAnimating
      _ -> 
        ngon 3 5 |> filled (rgb 100 0 200) |> move (20,-58)
          |> notifyTap StartAnimating
  ]

type alias Point = (Float,Float)

type LSystem  = X
              | F
-- and the operators are non-terminal expressions = branch nodes
              | Plus LSystem
              | Minus LSystem
              | Branch LSystem
              | Sequence (List LSystem)

-- function to apply the rules for our L-System
applyRules ls =
  case ls of
    X -> Sequence [ F
                  , Plus (Branch (Sequence [Branch X, (Minus X)]))
                  , Minus (Sequence [F, Branch(Sequence[Minus (Sequence[F, X])])])
                  , Plus X
                  ]
    F -> Sequence [F,F]
    Plus ls1 -> Plus (applyRules ls1) 
    Minus ls1 -> Minus (applyRules ls1) 
    Branch ls1 -> Branch (applyRules ls1)
    Sequence lsList -> Sequence (List.map applyRules lsList)

-- starting value for the L-System
start = X

-- apply the rules once:
gen1 = applyRules start

iterate n f x = if n < 1 then 
                  x
                else 
                  iterate (n-1) f x 
                    |> f

type alias TurtleState = ( Point 
                         , Point 
                         )

turtleDraw : Model -> LSystem -> List (Shape Msg)
turtleDraw colorChange lSystem =  
  let
    greenColor = hsl (degrees 122) 0.65 0.626
    darkGreenColor = hsl (degrees 117) 0.376 0.488
    --turtleDraw with a recursive helper function
    helper : Bool -> TurtleState -> LSystem 
         -> (TurtleState, List (Shape Msg))
    helper checkGen ( (x,y), (vx,vy) ) ls =
      case ls of
        X  -> -- for move forward
              let color = if checkGen then darkGreenColor else greenColor
                  leaf =
                    [
                       oval 7 18
                        |> filled (hsl (degrees 108) 0.494 0.578)
                        |> makeTransparent 0.65
                        |> rotate (degrees 25)
                        |> scale 0.35
                        |> move (x+vx, y+vy)
                    ]
              in
              --let color = hsl (degrees 117) 0.896 ((1/colorChange.time)*2 ) in
              ( ( (x+vx, y+vy)  
                , (vx,vy))      
              
              , [line (x,y) (x+vx, y+vy) |> outlined (solid 0.9) color] ++ leaf
              )
        F  -> -- this is also move forward, so the code is the same
              let color = if checkGen then darkGreenColor else greenColor
                  dot =[circle 0.6 |> filled (hsl (degrees 122) 0.952 0.207) |> move (x+vx, y+vy)]    
              in
              --let color = hsl (degrees 117) 0.896 ((1/colorChange.time)*2 ) in
              ( ((x+vx, y+vy), (vx,vy))
              , [line (x,y) (x+vx, y+vy) |> outlined (solid 0.9) color] ++ dot
              )
        Plus ls1 ->
          -- "+" means rotate, so we have to change the velocity when
          
          helper checkGen ( (x,y), rotL (vx,vy) ) ls1
        Minus ls1 ->
          helper  checkGen ( (x,y), rotR (vx,vy) ) ls1
        Branch ls1 ->
          -- when we get to the branch, we do all the same drawing,
          
          let
            (_,shapes) = helper checkGen ( (x,y), (vx,vy) ) ls1
          in
            ( ( (x,y), (vx,vy) ), shapes )
        Sequence lsList ->
          let
            recurseList rels turtleState lss =
              case lss of
                [lastLs] ->
                  let
                    (newState, shapes) = helper False turtleState lastLs
                  in
                    (newState, shapes)
                ls1 :: lsRest -> 
                  let 
                    (newState, shapes) = helper True turtleState ls1 
                    (finalState, moreShapes) = recurseList rels newState lsRest 
                  in
                    (finalState, shapes ++ moreShapes)
                [] -> (turtleState, [])
          in
            recurseList checkGen ( (x,y), (vx,vy) ) lsList
  in
    helper True ((0,0),(0,5)) lSystem
      |> Tuple.second 

rotL (x,y) = rot (degrees 25) (x,y)
rotR (x,y) = rot (degrees -25) (x,y)

-- general rotation
rot angle (x,y) = ( cos angle * x - sin angle * y
                  , sin angle * x + cos angle * y
                  )
                  
type Msg 
  = Tick Float GetKeyState
  | StartDragAt Point
  | MoveDragAt Point
  | StopDrag
  | StartAnimating
  | StopAnimating
  
type alias Model 
  = { time : Float
    , state : DragState
    , showPoints : Bool
    }

type DragState 
  = Waiting Float
  | Dragging Float
  | Animating Float
  
state2num state =
  case state of
    Waiting x -> x
    Dragging x -> x
    Animating x -> x
    
update : Msg -> Model -> Model
update msg model 
  = case msg of
      Tick t _   -> { model | time = t
                            , state = case model.state of 
                                Animating old -> 
                                  let new = old + t - model.time
                                  in if new > 5 then Waiting 5 else Animating new 
                                otherwise -> otherwise
                            }
      StartDragAt (x,_) -> 
        case model.state of 
          Waiting _ -> { model | state = Dragging (toGen x) } 
          _ -> model
      MoveDragAt (x,_) -> 
        case model.state of 
          Dragging _ -> { model | state = Dragging ( toGen x ) } 
          _ -> model
      StopDrag -> 
        case model.state of 
          Dragging generation -> { model | state = Waiting (toFloat <| round generation) } 
          _ -> model
      StartAnimating ->
        case model.state of 
          Waiting generation -> { model | state = Animating (if generation > 9.9 then 0 else generation) } 
          _ -> model
      StopAnimating ->
        case model.state of 
          Animating generation -> { model | state = Waiting generation } 
          _ -> model

-- convert mouse position to generation number and vice versa
toGen mouseX = 
  let
    raw = 0.1 * (mouseX + 50) 
  in
    if raw < 0 then 
      0
    else if raw > 5 then
      5
    else 
      raw
      
toPos genNum = 10 * genNum - 50

init : Model
init = { state = Waiting 0
       , time = 0
       , showPoints = False
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

hiddenEv1 f = \ t -> f 1