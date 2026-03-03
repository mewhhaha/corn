module Examples.ScenesNavigation
  ( runConcept
  ) where

import Prelude

import Data.List (intercalate)
import qualified Engine.Corn as Corn

data Route
  = MainMenu
  | Options
  | Game
  deriving (Eq, Show)

routeTableDef :: Corn.RouteTable Route
routeTableDef =
  Corn.routeTable
    [ (MainMenu, "/main-menu")
    , (Options, "/main-menu/options")
    , (Game, "/game")
    ]

instance Corn.RouteCodec Route where
  encodeRoute = Corn.encodeBy routeTableDef
  decodeRoute = Corn.decodeBy routeTableDef

data Msg
  = UiOpenOptions
  | UiCloseTop
  | UiStartGame
  | UiBackToMenu
  | UiForward
  deriving (Eq, Show)

data Model = Model
  { menuTicks :: !Int
  , optionsTicks :: !Int
  , gameTicks :: !Int
  } deriving (Eq, Show)

initialModel :: Model
initialModel =
  Model
    { menuTicks = 0
    , optionsTicks = 0
    , gameTicks = 0
    }

menuScene :: Corn.Scene Model Route Msg
menuScene =
  Corn.scene $ \_ inbox model0 ->
    let model1 = model0 {menuTicks = menuTicks model0 + 1}
        cmds =
          [Corn.Navigate (Corn.Push Options) | UiOpenOptions `elem` inbox]
            <> [Corn.Navigate (Corn.Push Game) | UiStartGame `elem` inbox]
            <> [Corn.Navigate Corn.Forward | UiForward `elem` inbox]
    in (model1, cmds)

optionsScene :: Corn.Scene Model Route Msg
optionsScene =
  Corn.scene $ \_ inbox model0 ->
    let model1 = model0 {optionsTicks = optionsTicks model0 + 1}
        cmds =
          [Corn.Navigate Corn.Back | UiCloseTop `elem` inbox]
    in (model1, cmds)

gameScene :: Corn.Scene Model Route Msg
gameScene =
  Corn.scene $ \_ inbox model0 ->
    let model1 = model0 {gameTicks = gameTicks model0 + 1}
        cmds =
          [Corn.Navigate Corn.Back | UiBackToMenu `elem` inbox]
    in (model1, cmds)

sceneFor :: Route -> Corn.Scene Model Route Msg
sceneFor routeId =
  case routeId of
    MainMenu -> menuScene
    Options -> optionsScene
    Game -> gameScene

game :: Corn.Game Route Model Msg
game = Corn.game MainMenu initialModel sceneFor

frameDt :: Double
frameDt = 0.016

scriptedInputs :: [[Msg]]
scriptedInputs =
  [ [UiOpenOptions]
  , [UiCloseTop]
  , [UiStartGame]
  , [UiBackToMenu]
  , [UiForward]
  ]

renderPath :: Corn.Runtime Route Model Msg -> String
renderPath runtime =
  let segs = map Corn.encodeRoute (Corn.currentPath runtime)
  in unlines
      [ "path=" <> show segs
      , "path-render=" <> intercalate " > " segs
      , "canGoBack=" <> show (Corn.canGoBack runtime) <> ", canGoForward=" <> show (Corn.canGoForward runtime)
      ]

renderTicks :: Model -> String
renderTicks model =
  "ticks: menu=" <> show (menuTicks model)
    <> ", options=" <> show (optionsTicks model)
    <> ", game=" <> show (gameTicks model)

runConcept :: IO ()
runConcept = go 1 (Corn.start game) scriptedInputs
  where
    go :: Int -> Corn.Runtime Route Model Msg -> [[Msg]] -> IO ()
    go _ _ [] = pure ()
    go frameIx runtime0 (inputs : rest) = do
      let (runtime1, outbox) = Corn.step frameDt inputs runtime0
          activeNow = map Corn.encodeRoute (Corn.currentPath runtime1)
      putStrLn ("frame " <> show frameIx)
      putStrLn ("  inputs=" <> show inputs)
      putStrLn ("  outbox=" <> show outbox)
      putStrLn ("  active=" <> show activeNow)
      putStrLn ("  " <> renderTicks (Corn.model runtime1))
      putStrLn (renderPath runtime1)
      go (frameIx + 1) runtime1 rest
