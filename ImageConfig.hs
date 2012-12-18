module ImageConfig where

import Graphics.UI.SDL

import Data.CircularList

import Control.Monad.State.Lazy

windowWidth  = 0 :: Int
windowheight = 0 :: Int
screenBpp    = 32 :: Int

textColor    = Color 0 0 0
font         = "DejaVuSansMono.ttf"
fontSize     = 15 :: Int

zoomStep     = 0.1 :: Double
moveStep     = 30 :: Int

data Modes = Fit | Zoom Double Surface | Full
           deriving (Eq)

data Config = Config {
      screen          :: Surface,
      imageList       :: CList String,
      currentImage    :: Surface,
      windowW         :: Int,
      windowH         :: Int,
      viewMode        :: Modes,
      offset          :: (Int, Int),
      infoText        :: Bool
}

type ConfState = StateT Config IO ()