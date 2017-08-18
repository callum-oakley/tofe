module UI (run) where

import Brick
  ( App(..), AttrName, BrickEvent(..), EventM, Next, Widget
  , attrMap, attrName, continue, defaultMain, fg, fill, hBox, halt
  , neverShowCursor, overrideAttr, str, vBox, vLimit, withAttr
  )
import Brick.Widgets.Border (border, borderAttr)
import Brick.Widgets.Center (center)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Graphics.Vty
  (Color(..), Event(..), Key(..), Modifier(..), defAttr)

import Tofe

emptyAttr :: AttrName
emptyAttr = attrName "empty"

doneAttr :: AttrName
doneAttr = attrName "done"

decorate :: Bool -> Tile -> Widget () -> Widget ()
decorate _ Nothing w = overrideAttr borderAttr emptyAttr $ border w
decorate d (Just n) w = style $ withAttr (attrName $ show n) $ border w
  where
    style = if d then overrideAttr borderAttr doneAttr else id

drawTile :: Bool -> Tile -> Widget ()
drawTile d tile = decorate d tile $ str $ padAndCenter 5 tile'
  where
    tile' = case tile of
      Nothing -> ""
      Just n -> show n
    padAndCenter w s = l w (length s) ++ s ++ r w (length s)
    l w m = replicate (ceiling $ (/ 2) $ fromIntegral $ w - m) ' '
    r w m = replicate (floor $ (/ 2) $ fromIntegral $ w - m) ' '

drawColumn :: Bool -> Column -> Widget ()
drawColumn d cols = vBox $ map (drawTile d) cols

drawBoard :: Bool -> Board -> Widget ()
drawBoard d b = hBox $ spaceCols colWidgets
  where
    spaceCols = intersperse (str " ")
    colWidgets = map (drawColumn d) $ columns b

drawScore :: Score -> Score -> Widget ()
drawScore current best = hBox
  [ str ("Score: " ++ show current)
  , vLimit 1 (fill ' ')
  , str ("Best: " ++ show best)
  ]

draw :: Score -> State -> [Widget ()]
draw best s = pure $ vBox
  [ drawScore (score s) best
  , center $ drawBoard (done s) (board s)
  ]

handleEvent :: State -> BrickEvent () e -> EventM () (Next State)
handleEvent s e = if done s then halt s else case e of
  (VtyEvent (EvKey key [])) -> liftIO s' >>= continue
    where
      s' = case key of
        KUp -> move North s
        KRight -> move East s
        KDown -> move South s
        KLeft -> move West s
        _ -> return s
  (VtyEvent (EvKey key [MCtrl])) -> case key of
    KChar 'c' -> halt s
    KChar 'd' -> halt s
    _ -> continue s
  _ -> continue s

app :: Score -> App State e ()
app best = App
  { appDraw = draw best
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr
    [ (emptyAttr, fg $ ISOColor 8)
    , (doneAttr, fg $ ISOColor 1)
    , (attrName "4", fg $ ISOColor 1)
    , (attrName "8", fg $ ISOColor 2)
    , (attrName "16", fg $ ISOColor 3)
    , (attrName "32", fg $ ISOColor 4)
    , (attrName "64", fg $ ISOColor 5)
    , (attrName "128", fg $ ISOColor 6)
    , (attrName "256", fg $ ISOColor 7)
    , (attrName "512", fg $ ISOColor 9)
    , (attrName "1024", fg $ ISOColor 10)
    , (attrName "2048", fg $ ISOColor 11)
    , (attrName "4096", fg $ ISOColor 12)
    , (attrName "8192", fg $ ISOColor 13)
    , (attrName "16384", fg $ ISOColor 14)
    , (attrName "32768", fg $ ISOColor 15)
    ]
  }

run :: Score -> IO Score
run best = do
  s <- defaultMain (app best) =<< initialState
  return $ score s
