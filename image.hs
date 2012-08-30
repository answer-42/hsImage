module Main where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import System.Environment (getArgs)
import Control.Monad (mapM, liftM)
import Data.CircularList

import ImagePaths (getImages)

windowWidth  = 0
windowheight = 0
screenBpp    = 32

textColor    = Color 0 0 0

zoomStep     = 0.1 

data Modes = Fit | Zoom Double Surface | Full
           deriving (Eq)

data Config = Config {
      screen          :: Surface,
      imageList       :: [String], -- It might be better to use Data.CircularList
      currentImagePos :: Int,
      currentImage    :: Surface,
      windowW         :: Int,
      windowH         :: Int,
      viewMode        :: Modes
}

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

loadAdjustedImage :: Config -> IO Config
loadAdjustedImage env = do
  oldImage <- loadImage $ (imageList env) !! (currentImagePos env)
  case (viewMode env) of     
    Fit             -> let ratio = fitImageRatio env{currentImage = oldImage}
                       in zoom oldImage ratio ratio False >>= \i -> return  env{currentImage = i}
    (Zoom ratio _)  -> return env{currentImage = oldImage, viewMode = Full}           
    _               -> return env{currentImage = oldImage}
             
applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

fitImageRatio :: Config -> Double
fitImageRatio env = if ratio1 < ratio2 then ratio1 else ratio2
    where
      intDiv x y = fromIntegral x /  fromIntegral y
      image  = currentImage env      
      ratio1 = windowW env `intDiv` surfaceGetWidth image
      ratio2 = windowH env `intDiv` surfaceGetHeight image
      
initEnv :: IO Config
initEnv = do
  args      <- getArgs
  screen    <- setVideoMode windowWidth windowheight screenBpp [Resizable]
  imageList <- liftM concat $ mapM getImages args
  image     <- loadImage $ imageList !! 0
  let windowW = surfaceGetWidth screen
  let windowH = surfaceGetHeight screen

  return (Config screen imageList 0 image windowW windowH Full)

changeImage op env = let n = ((currentImagePos env) `op` 1) `mod` (length $ imageList env)
                     in loadAdjustedImage env{currentImagePos = n} >>= loop
resize env w h = setVideoMode w h screenBpp [Resizable] 
                 >>= \s -> return env{windowW = w, windowH = h, screen  = s}
fitImage env = let ratio = fitImageRatio env
               in zoom (currentImage env) ratio ratio False
                      >>= \i -> loop env{viewMode = Fit, currentImage = i} 
fullImage env = loadImage (imageList env !! currentImagePos env)
                   >>= \i -> loop env{viewMode = Full, currentImage = i}

zoomWith op env@Config {imageList=images, currentImagePos=n,viewMode=(Zoom ratio origImage)} = 
    do let newRatio = (ratio `op` zoomStep)
       newImage <- zoom origImage newRatio newRatio False
       loop env{viewMode = (Zoom newRatio origImage), currentImage = newImage}                                  
zoomWith op env = zoomWith op env{viewMode = Zoom 1 (currentImage env)}

checkEvent :: Event-> Config -> IO ()
checkEvent event env = case event of 
                             (KeyDown (Keysym key _ _)) -> 
                                 case key of
                                   SDLK_ESCAPE -> return ()
                                   SDLK_RIGHT  -> changeImage (+) env
                                   SDLK_LEFT   -> changeImage (-) env
                                   SDLK_f      -> fitImage env
                                   SDLK_s      -> fullImage env
                                   SDLK_i      -> zoomWith (+) env
                                   SDLK_o      -> zoomWith (-) env
--                                   SDLK_d      -> moveLeft env
                                   _           -> loop env
                             (VideoResize width height) -> resize env width height >>= fitImage
                             _ -> loop env

loop :: Config -> IO ()
loop env = do      
      -- Clear screen
      fillRect (screen env) Nothing (Pixel 0)
      applySurface 0 0 (currentImage env) (screen env)

      Graphics.UI.SDL.flip (screen env)
      event <- waitEventBlocking  
      checkEvent event env
        
main = withInit [InitEverything] $ do
         env <- initEnv
         TTFG.init
         loop env
  


      --font <- openFont "DejaVuSansMono.ttf" 15
      --infoText <- renderTextSolid font "Testhbc" textColor 
      --applySurface 5 5 infoText (screen env)
