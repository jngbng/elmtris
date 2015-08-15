-- module Tetris where

import Audio
import Audio exposing (defaultTriggers)
import Mouse
import Util exposing  (..)
import Tetromino exposing (..)
import TetrisColor exposing (..)
import Board exposing (..)
import Control exposing (..)
import Control
import Dict exposing (Dict, fromList, member)
import Dict
import Keyboard exposing (arrows, keysDown)
import Random
import Char exposing (toCode, fromCode)
import Graphics.Element as G exposing (spacer, midTop, container, down, flow, leftAligned, middle)
import Graphics.Collage as C exposing (collage, move, group)
import Text exposing (bold)
import Time exposing (every, second, inSeconds)
import Signal exposing ((<~), foldp)
import List exposing (map, head, tail, intersperse, foldr)
import Set
import Debug
                      
type alias Piece = (Tetromino, TetrisColor)

width = 300
height = 2*width
fwidth = toFloat width
fheight = toFloat height
panelWidth = width // 3
panelHeight = height

blockSize = width // 10
fblockSize = toFloat blockSize
tapDelay = 0.08
maxMovesPerSecond = 100
fps = 200
setDelay = 0.5

restartKey : Keyboard.KeyCode
restartKey = toCode 'R'

pauseKey : Keyboard.KeyCode
pauseKey = toCode 'P'

hardDropKey : Keyboard.KeyCode
hardDropKey = toCode ' '

holdKey : Keyboard.KeyCode
holdKey = toCode 'X'

toggleMusicKey : Keyboard.KeyCode
toggleMusicKey = toCode 'M'

(.) = (<<)

pieceDict : Dict Int Piece
pieceDict = fromList . zip [0..6] <| pieces
pieces : List Piece
pieces = 
  zip (map (shift (4, 0)) [line, square, zpiece, spiece, jpiece, lpiece, tpiece])
      [Red, Orange, Yellow, Green, Blue, Indigo, Violet]

defaultPiece = (shift (4,0) line, Red)

getPiece : Int -> Piece
getPiece n = Dict.get n pieceDict |> Maybe.withDefault defaultPiece

type alias GameState = {board:Board,
         init:Bool,
         falling:Piece, 
         preview:List Piece,
         hold: Maybe Piece,
         canHold:Bool,
         tap:Bool,
         tapped:(Bool, Float),
         keyDelay:Bool,
         forceDelay:Bool,
         setDelay:Float,
         timestamp:Float,
         level:Float, 
         score:Int,
         lines:Int,
         tick:Float,
         set:Bool,
         paused:Bool,
         gameover:Bool,
         music:Bool,
         click:Bool,
         dropSound:Bool,
         keys:List Keyboard.KeyCode
       }

game : GameState
game = { board=emptyBoard, 
         init=True,
         falling=(getPiece 0), 
         preview=[],
         hold=Nothing,
         canHold=True,
         tap=False,
         tapped= (False, 0),
         keyDelay=False,
         forceDelay=False,
         setDelay = 0,
         timestamp=0,
         level=1, 
         score=0,
         lines=0,
         tick=0,
         set=False,
         paused=True,
         gameover=False,
         music=True,
         click=False,
         dropSound=False,
         keys=[]
       }

getPoints x =
  case x of
    1 -> 25
    2 -> 100
    3 -> 400
    4 -> 1000
    _ -> 0

type alias GameSignal = ( {x : Int, y: Int}, List Keyboard.KeyCode, Time.Time, Int, List Int)

handle : GameSignal -> GameState -> GameState
handle (arrow, keys, t, next, init) = smoothControl t keys . cleanup keys . setPiece next t . autoDrop t . arrowControls arrow . keyControls keys . hold keys next . startup init . restartGame keys . pause keys . toggleMusic keys . dropSound keys . clear

clear game = {game | click <- False, dropSound <- False}

hold : List Keyboard.KeyCode -> Int -> GameState -> GameState
hold ks n game = 
  let doHold = List.member holdKey ks in
  if doHold then (swapHold (getPiece n) game) else game


dropSound keys g =
  if g.forceDelay then g else
    if (List.member hardDropKey keys) then {g | dropSound <- True} else g

restartGame keys g =
  if g.forceDelay then g else
  if (List.member restartKey keys) then {game| paused <- False, forceDelay <- True} else g

pause keys game =
  if game.forceDelay then game else
  if (List.member pauseKey keys) then {game| paused <- not game.paused, forceDelay <- True} else game

toggleMusic keys game =                                                   
  if game.forceDelay then game else
  if (List.member toggleMusicKey keys) then {game| music <- not game.music, forceDelay <- True} else game


swapHold : Piece -> GameState -> GameState
swapHold piece game = 
  case game.canHold of
    False -> game
    True ->
      let next = {game| hold <- (Just . reset <| game.falling),
                        canHold <- False} in
      case game.hold of
        Nothing -> {next| falling <- (head game.preview |> Maybe.withDefault defaultPiece), 
                          preview <- ((tail game.preview |> Maybe.withDefault []) ++ [piece])}
        Just held -> {next| falling <- held}

reset : Piece -> Piece
reset (tr, color) =
  let ((minX, minY), (_, maxY)) = bounds tr in
  let width = 1 + maxY - minY in
  let center = (boardWidth // 2) - (width // 2) in
  let tr' = shift (center, 0) . shift (-minX, -minY) <| tr in
  (tr', color)

startup (p::pieces) game =
  case game.init of
    False -> game
    True -> 
      let falling = getPiece p in
      let preview = map getPiece pieces in
      {game | init <- False, falling <- falling, preview <- preview}

smoothControl t ks game =
  case ks of
    [] -> {game | keyDelay <- False, timestamp <- inSeconds t, forceDelay <- False, tap <- False, tapped <- (False, 0)}
    _ ->
      let time = inSeconds t in
        doKeyDelay time . doTapDelay time <| game

doTapDelay time game = 
   case game.tapped of
     (False, _) -> {game| tapped <- (True, time)}
     (True, at) -> 
        let tap = (time - at) < tapDelay in
        {game| tap <- tap}

doKeyDelay time game =
        let wait = (time - game.timestamp) < (1/maxMovesPerSecond) in
        if wait 
           then {game | keyDelay <- True} 
           else {game | keyDelay <- False, timestamp <- time}

cleanup keys game =
  let (board', cleared) = clearBoard game.board in
  let points = getPoints cleared in
  let score = game.score + points in
  let lines = game.lines + cleared in
  let level = toFloat <| (lines // 10) + 1 in
  {game| board <- board', 
         score <- score, 
         lines <- lines, 
         level <- level,
         keys <- keys}

autoDrop t game =
  let time = (inSeconds t) in
  let drop = (time - game.tick) > (1/game.level) in
  let next = forceControl Drop game in
    case drop of
      False -> game
      True ->
       let set = checkSet . toGameState <| game in
       let delay = if (set && not game.set) then time+setDelay else game.setDelay in
       {next | tick <- time, set <- set, setDelay <- delay}

setPiece n t game =
  case game.set of
    False -> game
    True ->
      if not . checkSet . toGameState <| game then game else
      if (game.setDelay > (inSeconds t)) then game else
      let next = head game.preview |> Maybe.withDefault defaultPiece in
      let preview = (tail game.preview |> Maybe.withDefault []) ++ [getPiece n] in
      let board' = insertTetromino (game.falling) (game.board) in
      let game' = {game | board <- board', falling <- next, preview <- preview} in
      let gameover = not . isValidState . toGameState <| game' in
      {game'| timestamp <- (inSeconds t), set <- False, canHold <- True, gameover <- gameover}

toGameState game = (game.board, fst <| game.falling)

getLevel : Int -> Float
getLevel n = toFloat <| (n // 10) + 1

keyControls ks game = 
  foldr doControl game (map getKeyControl ks)
    
getKeyControl : Int -> Maybe Control
getKeyControl k =
   if (k == hardDropKey) then Just HardDrop else Nothing

arrowControls arr game = 
  let x = arr.x in
  let y = arr.y in
  let game' = foldr doControl game <| getArrowControl (x, y) in
  if y == 1 then {game'| click <- True} else game'
      
getArrowControl : (Int, Int) -> List (Maybe Control)
getArrowControl (x, y) =  
  let moveX = 
        case x of
          -1 -> Just MoveLeft
          1 -> Just MoveRight
          0 -> Nothing
  in
   let moveY =
         case y of
           -1 -> Just Drop
           1 -> Just . Rotate <| CW
           0 -> Nothing
   in
    [moveX, moveY]
  
forceControl c game =  
  if game.paused then game else
  let board = game.board in
  let (tr, color) = game.falling in
  let (board', tr') = control (board, tr) c in
  {game | board <- board', falling <- (tr', color)}
  
doControl c game =
  if (game.paused || game.keyDelay || game.tap) then game else
  case c of
   Nothing -> game
   Just c ->
     if (isForcedDelay c && game.forceDelay) then game else
      let game' = forceControl c game in
      let game'' = if (isForcedDelay c) then {game' | forceDelay <- True} else game' in
      if (isSetControl c) then {game'' | set <- True} else game''

isForcedDelay c =
  case c of
    HardDrop -> True
    Rotate _ -> True
    _ -> False

isSetControl c =
  case c of
    HardDrop -> True
    _ -> False

plainText = Text.fromString >> leftAligned
asText = toString >> Text.fromString >> leftAligned         

label l r = flow G.right [plainText l, spacer 5 5, asText r]
  

scoreBoard game = 
  let board = flow down [label "Score: " game.score,
                         label "Level: " game.level,
                         label "Lines: " game.lines]
  in
   collage panelWidth 100 . (flip (::) []) . C.toForm <| board
            
render game =
  let withPiece = insertTetromino (game.falling) (game.board) in
  let boardDisplay = asElement withPiece blockSize in
  let boardWithShadow = shadow (game.falling) (game.board) boardDisplay in
  if game.paused then pauseScreen game else
    if game.gameover then gameoverScreen game else
  flow down [spacer 10 10, 
             flow G.right [holdBoard game, spacer 10 10, 
                           boardWithShadow, spacer 10 10, 
                           previewBoard game]]

pauseScreen game = 
  let w = width+2*panelWidth in
  let h = height in
  let elem = container w h middle <| pauseScreenText game in
  let form = collage w h [C.toForm elem] in
  form

gameoverScreen game = 
  let w = width+2*panelWidth in
  let h = height in
  let elem = container w h middle <| gameoverScreenText game in
  let form = collage w h [C.toForm elem] in
  form

gameoverScreenText game = 
  let w = width in
  let h = 30 in
  let contents = flow down [label "Score: " game.score, 
                          label "Level: " game.level,
                          label "Lines: " game.lines] in
  let title = leftAligned . Text.height 28 . bold . Text.fromString <| "Game Over" in
  flow down [spacer 10 10, title, spacer 50 50, contents, plainText "Press R to play again"]

pauseScreenText game = 
  let w = width in
  let h = 30 in
  let format = map (container w h G.topLeft . plainText) in
  let contents = flow down . format <|
                 ["Left, Down, Right Arrow - Move",
                  "Up Arrow - Rotate",
                  "Space - Drop",
                  "X - Hold / Swap current piece",
                  "P - Toggle this screen",
                  "M - Toggle Music",
                  "R - New Game"] in
  let title = leftAligned . Text.height 28 . bold . Text.fromString <| "Elmtris" in
  flow down [spacer 10 10, title, spacer 50 50, contents]


shadow (tr, color) board boardDisplay =
  let (_, shadow) = hardDrop (board, tr) in
  let ((minX, minY), _) = bounds shadow in
  let offset = (-(fwidth/2)+(fblockSize/2), (fheight/2)-(fblockSize/2)) in
  let offset' = ((toFloat minX) * fblockSize, -(toFloat minY) * fblockSize) in
  let elem = move offset' . move offset <| pieceToForm fblockSize (shadow, Shadow) in
  let asForm = C.toForm boardDisplay in
  collage width height [asForm, elem]



previewBoard game =
  let preview = flow down . intersperse (spacer 10 10) . map pieceToElement <| (game.preview) in
  container panelWidth panelHeight midTop <| flow down [spacer 10 10, plainText "Next", spacer 10 10, preview, spacer 10 10, scoreBoard game]

holdBoard game =
  let held = 
        case game.hold of
          Nothing -> plainText "Press 'x'"
          Just x -> pieceToElement x
  in
   let lines = collage panelWidth 30 . (flip (::) []) . C.toForm . asText <| game.lines in
  container panelWidth panelHeight midTop <| flow down [spacer 10 10, plainText "Holding", spacer 10 10, held]

pieceToForm fblockSize (tr, color) =
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let translate = shift (-minX, -minY) tr in
  let blocks = map (flip (toForm fblockSize) color) translate in
  group blocks

pieceToElement (tr, color) =
  let fblockSize' = fblockSize/2 in
  let blockSize' = floor fblockSize' in
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let width = (1+maxX-minX)*blockSize' in
  let height = (1+maxY-minY)*blockSize' in
  let offset = (-((toFloat width)/2-(fblockSize'/2)), (toFloat height-(fblockSize'/2))/2) in
  let piece =  move offset <| pieceToForm fblockSize' (tr, color) in
  collage (width+10) (height+10) [piece]

ticker = every <| Time.second/fps

range low high sig =
  Signal.map (\time ->
                let seed = Random.initialSeed (round time) in
                let gen = Random.int low high in
                Random.generate gen seed |> fst) sig

inputSignal : Signal GameSignal
inputSignal = Signal.map5 (,,,,) arrows (Signal.map Set.toList keysDown) ticker (range 0 6 ticker) (randoms 6 0 6 ticker)

piece = rotate CW <| shift (0,1) zpiece

-- Deprecated function
combine : List (Signal a) -> Signal (List a)
combine signals =
    List.foldr (Signal.map2 (::)) (Signal.constant []) signals

randoms n low high sig = combine <| randoms' n low high sig

randoms' n low high sig =
  if n <= 0 then [] else (range low high sig)::(randoms' (n-1) low high sig)  

handleTheme g = if g.music && (not g.paused) then Audio.Play else Audio.Pause

theme =
    let props p = if p.currentTime > 37.6 then Just (Audio.Seek 0.05) else Nothing 
        builder = { src = "snd/theme.mp3",
                    triggers = {defaultTriggers | timeupdate <- True},
                    propertiesHandler = props,
                    actions = handleTheme <~ mainSignal }
    in Audio.audio builder

handleClick g = if g.music && g.click && (not g.paused) then Audio.Play else Audio.NoChange

click =
    let builder = { src = "snd/click.wav",
                    triggers = defaultTriggers,
                    propertiesHandler = (\_ -> Nothing),
                    actions = handleClick <~ mainSignal }
    in Audio.audio builder

handleSwap g = if g.music && g.dropSound && (not g.paused) then Audio.Play else Audio.NoChange
swap =
    let builder = { src = "snd/woosh.wav",
                    triggers = defaultTriggers,
                    propertiesHandler = (\_ -> Nothing),
                    actions = handleSwap <~ mainSignal }
    in Audio.audio builder

mainSignal : Signal GameState
mainSignal = foldp handle game inputSignal

main = render <~ mainSignal
