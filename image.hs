module Main where

{- 
   Add error checking:
   * Loading files -> stop with meaningful error if no image loaded
-}

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import System.Environment (getArgs)
import Control.Monad (mapM, liftM)
import Data.CircularList
import Data.Maybe

import ImagePaths (getImages)

windowWidth  = 0
windowheight = 0
screenBpp    = 32

textColor    = Color 0 0 0

zoomStep     = 0.1 
moveStep     = 30

data Modes = Fit | Zoom Double Surface | Full
           deriving (Eq)

data Config = Config {
      screen          :: Surface,
      imageList       :: CList String,
      currentImage    :: Surface,
      windowW         :: Int,
      windowH         :: Int,
      viewMode        :: Modes,
      offset         :: (Int, Int)
}
 
loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

loadAdjustedImage :: Config -> IO Config
loadAdjustedImage env = do
  oldImage <- loadImage $ fromJust $ focus $ imageList env
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

changeImage :: (CList String -> CList String) -> Config -> IO Config       
changeImage op env = loadAdjustedImage env{imageList = op $ imageList env}

resize :: Config -> Int -> Int -> IO Config
resize env w h = do s <- setVideoMode w h screenBpp [Resizable] 
                    return env{windowW = w, windowH = h, screen  = s}

fitImage :: Config -> IO Config
fitImage env = do let ratio = fitImageRatio env
                  i <- zoom (currentImage env) ratio ratio False
                  return env{viewMode = Fit, currentImage = i, offset = (0,0) } 

fullImage :: Config -> IO Config
fullImage env = do i <- loadImage (fromJust . focus . imageList $ env)
                   return env{viewMode = Full, currentImage = i}

zoomWith :: (Double -> Double -> Double) -> Config -> IO Config
zoomWith op env@Config {imageList=images, viewMode=(Zoom ratio origImage)} = 
    do let newRatio = (ratio `op` zoomStep)
       newImage <- zoom origImage newRatio newRatio False
       return env{viewMode = (Zoom newRatio origImage), currentImage = newImage}                                  
zoomWith op env = zoomWith op env{viewMode = Zoom 1 (currentImage env)}

moveHor :: (Int -> Int -> Int) -> Config -> IO Config
moveHor op env = let oldOffset = offset env 
                 in return env{offset =  ((fst oldOffset) `op` moveStep, snd oldOffset)} 

moveVer :: (Int -> Int -> Int) -> Config -> IO Config
moveVer op env = let oldOffset = offset env 
                 in return env{offset =  (fst oldOffset, (snd oldOffset) `op` moveStep)} 


checkEvent :: Event-> Config -> IO ()
checkEvent event env = do env <- (case event of 
                                    (KeyDown (Keysym key _ _)) -> 
                                        case key of
                                          SDLK_ESCAPE -> return env{imageList=empty} 
                                          SDLK_RIGHT  -> changeImage rotR env
                                          SDLK_LEFT   -> changeImage rotL env
                                          SDLK_f      -> fitImage env
                                          SDLK_v      -> fullImage env
                                          SDLK_i      -> zoomWith (+) env
                                          SDLK_o      -> zoomWith (-) env
                                          SDLK_a      -> moveHor (-) env -- move left
                                          SDLK_d      -> moveHor (+) env -- move right
                                          SDLK_w      -> moveVer (-) env -- move up
                                          SDLK_s      -> moveVer (+) env -- move down
                                          _           -> return env
                                    (VideoResize width height) -> resize env width height >>= fitImage
                                    _ -> return env)
                          loop env

initEnv :: IO Config
initEnv = do
  args      <- getArgs
  screen    <- setVideoMode windowWidth windowheight screenBpp [Resizable]
  imageList <- liftM (fromList . concat) $ mapM getImages args
  image     <- loadImage $ fromJust $ focus imageList
  let windowW = surfaceGetWidth screen
      windowH = surfaceGetHeight screen

  return (Config screen imageList image windowW windowH Full (0,0))

loop :: Config -> IO ()
loop env = do      
      -- Clear screen
      fillRect (screen env) Nothing (Pixel 0)
      applySurface (fst $ offset env) (snd $ offset env) (currentImage env) (screen env)
      
      Graphics.UI.SDL.flip (screen env)
      event <- waitEventBlocking  
      if isEmpty $ imageList env
      then return ()
      else checkEvent event env

main :: IO ()        
main = withInit [InitEverything] $ do
         env <- initEnv
         TTFG.init >> loop env
         


      --font <- openFont "DejaVuSansMono.ttf" 15
      --infoText <- renderTextSolid font "Testhbc" textColor 
      --applySurface 5 5 infoText (screen env)
