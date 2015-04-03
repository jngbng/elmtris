module Tetris where

-- import Audio
-- import Audio(defaultTriggers)
import Mouse
import Util (..)
import Tetromino (..)
import TetrisColor (..)
import Board (..)
import Control (..)
import Control
import Dict (Dict, fromList, member)
import Dict
import List
import List ((::))
import Keyboard (arrows, keysDown)
import Random
import Char (toCode, fromCode)
import Graphics.Element as G
import Graphics.Collage as C
import Text
import Time (inSeconds, every, second)
import Signal
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

restartKey : Int
restartKey = toCode 'r'

pauseKey : Int
pauseKey = toCode 'p'

hardDropKey : Int
hardDropKey = toCode ' '

holdKey : Int
holdKey = toCode 'x'

toggleMusicKey : Int
toggleMusicKey = toCode 'm'

pieceDict : Dict Int Piece
pieceDict = fromList << List.map2 (,) [0..6] <| pieces
pieces : List Piece
pieces = 
  List.map2 (,) (List.map (shift (4, 0)) [line, square, zpiece, spiece, jpiece, lpiece, tpiece])
      [Red,  Orange, Yellow, Green,  Blue,   Indigo, Violet]

getPiece : Int -> Piece
getPiece n = case Dict.get n pieceDict of
               Just p -> p
               Nothing -> List.head pieces

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
--          click=False,
         dropSound=False
       }

getPoints x =
  case x of
    1 -> 25
    2 -> 100
    3 -> 400
    4 -> 1000
    _ -> 0

handle (arrow, keys, t, next, init) = smoothControl t keys << cleanup keys << setPiece next t << autoDrop t << arrowControls arrow << keyControls keys << hold keys next << startup init << restartGame keys << pause keys << toggleMusic keys << dropSound keys << clear

clear game = {game | click <- False, dropSound <- False}
-- clear game = {game | click <- False, dropSound <- False}

hold ks n game = 
  let doHold = List.any ((==) holdKey) ks in
  if doHold then (swapHold (getPiece n) game) else game


dropSound keys g =
  if g.forceDelay then g else
    if (List.any ((==)hardDropKey) keys) then {g | dropSound <- True} else g

restartGame keys g =                                                   
  if g.forceDelay then g else
  if (List.any ((==)restartKey) keys) then {game| paused <- False, forceDelay <- True} else g

pause keys game =                                                   
  if game.forceDelay then game else
  if (List.any ((==)pauseKey) keys) then {game| paused <- not game.paused, forceDelay <- True} else game

toggleMusic keys game =                                                   
  if game.forceDelay then game else
  if (List.any ((==)toggleMusicKey) keys) then {game| music <- not game.music, forceDelay <- True} else game


swapHold piece game = 
  case game.canHold of
    False -> game
    True ->
      let next = {game| hold <- (Just << reset <| game.falling),
                        canHold <- False} in
      case game.hold of
        Nothing -> {next| falling <- (List.head game.preview), 
                          preview <- ((List.tail game.preview) ++ [piece])}
        Just held -> {next| falling <- held}
                     
reset (tr, color) =
  let ((minX, minY), (_, maxY)) = bounds tr in
  let width = 1 + maxY - minY in
  let center = (boardWidth // 2) - (width // 2) in
  let tr' = shift (center, 0) << shift (-minX, -minY) <| tr in
  (tr', color)

startup (p::pieces) game =
  case game.init of
    False -> game
    True -> 
      let falling = getPiece p in
      let preview = List.map getPiece pieces in
      {game | init <- False, falling <- falling, preview <- preview}

smoothControl t ks game =
  case ks of
    [] -> {game | keyDelay <- False, timestamp <- inSeconds t, forceDelay <- False, tap <- False, tapped <- (False, 0)}
    _ ->
      let time = inSeconds t in
        doKeyDelay time << doTapDelay time <| game

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
       let set = checkSet << toGameState <| game in
       let delay = if (set && not game.set) then time+setDelay else game.setDelay in
       {next | tick <- time, set <- set, setDelay <- delay}

setPiece n t game =
  case game.set of
    False -> game
    True ->
      if not << checkSet << toGameState <| game then game else
      if (game.setDelay > (inSeconds t)) then game else
      let next = List.head game.preview in
      let preview = (List.tail game.preview) ++ [getPiece n] in
      let board' = insertTetromino (game.falling) (game.board) in
      let game' = {game | board <- board', falling <- next, preview <- preview} in
      let gameover = not << isValidState << toGameState <| game' in
      {game'| time <- (inSeconds t), set <- False, canHold <- True, gameover <- gameover}

toGameState game = (game.board, fst <| game.falling)

getLevel : Int -> Float
getLevel n = toFloat <| (n // 10) + 1

keyControls ks game = 
  List.foldr doControl game (List.map getKeyControl ks)
    
getKeyControl : Int -> Maybe Control
getKeyControl k =
   if (k == hardDropKey) then Just HardDrop else Nothing

arrowControls arr game = 
  let x = arr.x in
  let y = arr.y in
  let game' = List.foldr doControl game <| getArrowControl (x, y) in
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
           1 -> Just << Rotate <| CW
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

label l r = G.flow G.right [Text.plainText l, G.spacer 5 5, Text.asText r]
  

scoreBoard game = 
  let board = G.flow G.down [label "Score: " game.score,
                         label "Level: " game.level,
                         label "Lines: " game.lines]
  in
   C.collage panelWidth 100 << (flip (::) []) << C.toForm <| board
            
render game =
  let withPiece = insertTetromino (game.falling) (game.board) in
  let boardDisplay = asElement withPiece blockSize in
  let boardWithShadow = shadow (game.falling) (game.board) boardDisplay in
  if game.paused then pauseScreen game else
    if game.gameover then gameoverScreen game else
  G.flow G.down [G.spacer 10 10, 
             G.flow G.right [holdBoard game, G.spacer 10 10, 
                           boardWithShadow, G.spacer 10 10, 
                           previewBoard game]]

pauseScreen game = 
  let w = width+2*panelWidth in
  let h = height in
  let elem = G.container w h G.middle <| pauseScreenText game in
  let form = C.collage w h [C.toForm elem] in
  form

gameoverScreen game = 
  let w = width+2*panelWidth in
  let h = height in
  let elem = G.container w h G.middle <| gameoverScreenText game in
  let form = C.collage w h [C.toForm elem] in
  form

gameoverScreenText game = 
  let w = width in
  let h = 30 in
  let contents = G.flow G.down [label "Score: " game.score, 
                          label "Level: " game.level,
                          label "Lines: " game.lines] in
  let title = Text.leftAligned << Text.height 28 << Text.bold << Text.asText <| "Game Over" in
  G.flow G.down [G.spacer 10 10, title, G.spacer 50 50, contents, Text.plainText "Press R to play again"]

pauseScreenText game = 
  let w = width in
  let h = 30 in
  let format = List.map (G.container w h G.topLeft << Text.plainText) in
  let contents = G.flow G.down << format <|
                 ["Left, Down, Right Arrow - Move",
                  "Up Arrow - Rotate",
                  "Space - Drop",
                  "X - Hold / Swap current piece",
                  "P - Toggle this screen",
                  "M - Toggle Music",
                  "R - New Game"] in
  let title = Text.leftAligned << Text.height 28 << Text.bold << Text.asText <| "Elmtris" in
  G.flow G.down [G.spacer 10 10, title, G.spacer 50 50, contents]


shadow (tr, color) board boardDisplay =
  let (_, shadow) = hardDrop (board, tr) in
  let ((minX, minY), _) = bounds shadow in
  let offset = (-(fwidth/2)+(fblockSize/2), (fheight/2)-(fblockSize/2)) in
  let offset' = ((toFloat minX) * fblockSize, -(toFloat minY) * fblockSize) in
  let elem = C.move offset' << C.move offset <| pieceToForm fblockSize (shadow, Shadow) in
  let asForm = C.toForm boardDisplay in
  C.collage width height [asForm, elem]



previewBoard game =
  let preview = G.flow G.down << List.intersperse (G.spacer 10 10) << List.map pieceToElement <| (game.preview) in
  G.container panelWidth panelHeight G.midTop <| G.flow G.down [G.spacer 10 10, Text.plainText "Next", G.spacer 10 10, preview, G.spacer 10 10, scoreBoard game]

holdBoard game =
  let held = 
        case game.hold of
          Nothing -> Text.plainText "Press 'x'"
          Just x -> pieceToElement x
  in
   let lines = C.collage panelWidth 30 << (flip (::) []) << C.toForm << Text.asText <| game.lines in
  G.container panelWidth panelHeight G.midTop <| G.flow G.down [G.spacer 10 10, Text.plainText "Holding", G.spacer 10 10, held]

pieceToForm fblockSize (tr, color) =
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let translate = shift (-minX, -minY) tr in
  let blocks = List.map (flip (toForm fblockSize) color) translate in
  C.group blocks

pieceToElement (tr, color) =
  let fblockSize' = fblockSize/2 in
  let blockSize' = floor fblockSize' in
  let ((minX, minY), (maxX, maxY)) = bounds tr in
  let width = (1+maxX-minX)*blockSize' in
  let height = (1+maxY-minY)*blockSize' in
  let offset = (-((toFloat width)/2-(fblockSize'/2)), (toFloat height-(fblockSize'/2))/2) in
  let piece =  C.move offset <| pieceToForm fblockSize' (tr, color) in
  C.collage (width+10) (height+10) [piece]

ticker = every <| second/fps

inputSignal = Signal.map5 (,,,,) arrows keysDown ticker (Random.int 0 6 ticker) (randoms 6 0 6 ticker)

piece = rotate CW <| shift (0,1) zpiece

randoms n low high sig = combine <| randoms' n low high sig

randoms' n low high sig =
  if n <= 0 then [] else (Random.int low high sig)::(randoms' (n-1) low high sig)  

-- handleTheme g = if g.music && (not g.paused) then Audio.Play else Audio.Pause

-- theme =
--     let props p = if p.currentTime > 37.6 then Just (Audio.Seek 0.05) else Nothing 
--         builder = { src = "snd/theme.mp3",
--                     triggers = {defaultTriggers | timeupdate <- True},
--                     propertiesHandler = props,
--                     actions = handleTheme <~ mainSignal }
--     in Audio.audio builder

-- handleClick g = if g.music && g.click && (not g.paused) then Audio.Play else Audio.NoChange

-- click =
--     let builder = { src = "snd/click.wav",
--                     triggers = defaultTriggers,
--                     propertiesHandler = (\_ -> Nothing),
--                     actions = handleClick <~ mainSignal }
--     in Audio.audio builder

-- handleSwap g = if g.music && g.dropSound && (not g.paused) then Audio.Play else Audio.NoChange
-- swap =
--     let builder = { src = "snd/woosh.wav",
--                     triggers = defaultTriggers,
--                     propertiesHandler = (\_ -> Nothing),
--                     actions = handleSwap <~ mainSignal }
--     in Audio.audio builder

mainSignal = foldp handle game inputSignal

main = render <~ mainSignal
