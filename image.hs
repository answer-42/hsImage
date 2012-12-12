module Main where

{- 
   * Add error checking:
   ** Loading files -> stop with meaningful error if no image loaded
   * Restriced movement space in moveHor and moveVer
-}

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer

-- import qualified Graphics.UI.SDL.TTF.General as TTFG
-- import Graphics.UI.SDL.TTF.Management
-- import Graphics.UI.SDL.TTF.Render

import System.Environment (getArgs)
import System.FilePath (takeFileName)

import Control.Monad (mapM, liftM, unless)
import Control.Monad.State
--import Control.Monad.Identity

import Data.CircularList
import Data.Maybe

import ImagePaths (getImages)
import ImageConfig

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
 
loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

loadAdjustedImage :: Config -> IO Config
loadAdjustedImage env = do
  oldImage <- loadImage $ fromJust $ focus $ imageList env
  case viewMode env of     
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
      intDiv :: Int -> Int -> Double
      intDiv x y = fromIntegral x /  fromIntegral y
      image  = currentImage env      
      ratio1 = windowW env `intDiv` surfaceGetWidth image
      ratio2 = windowH env `intDiv` surfaceGetHeight image

changeImage :: (CList String -> CList String) -> Config -> IO Config       
changeImage op env = loadAdjustedImage env{imageList = op $ imageList env, offset = (0,0)}

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
    do let newRatio = ratio `op` zoomStep
       newImage <- zoom origImage newRatio newRatio False
       return env{viewMode = Zoom newRatio origImage, currentImage = newImage}                                  
zoomWith op env = zoomWith op env{viewMode = Zoom 1 (currentImage env)}

moveHor :: (Int -> Int -> Int) -> Config -> IO Config
moveHor op env = let oldOffset = offset env
                     newOffset = (fst oldOffset `op` moveStep, snd oldOffset)
                 in return env{offset = newOffset} 

moveVer :: (Int -> Int -> Int) -> Config -> IO Config
moveVer op env = let oldOffset = offset env 
                 in return env{offset = (fst oldOffset, snd oldOffset `op` moveStep)} 

-- switchInfo :: Config -> IO Config
-- switchInfo env = return env{infoText = not $ infoText env}


checkEvent :: Event-> Config -> IO ()
checkEvent event env = do env <- case event of 
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
   --                                       SDLK_t      -> switchInfo env
                                          _           -> return env
                                    (VideoResize width height) -> resize env width height >>= fitImage
                                    _ -> return env
                          loop env

initEnv :: IO Config
initEnv = do
  args      <- getArgs
  screen    <- setVideoMode windowWidth windowheight screenBpp [Resizable]
  imageList <- liftM (fromList . concat) $ mapM getImages args
  image     <- loadImage $ fromJust $ focus imageList
  let windowW = surfaceGetWidth screen
      windowH = surfaceGetHeight screen
  return (Config screen imageList image windowW windowH Full (0,0) False)

loop :: Config -> IO ()
loop env = do
      -- Clear screen
      fillRect (screen env) Nothing (Pixel 0)
      uncurry applySurface (offset env) (currentImage env) (screen env)
      
      {-
        We have to abstract this nonsense away and add some more useull output ;)
 
      -}
      {-
      if infoText env
      then do font <- openFont font fontSize
              let fn = takeFileName $ fromJust $ focus $ imageList env 
              infoText <- renderTextSolid font fn textColor 
              applySurface 5 5 infoText (screen env)
      else return False 
      -}

      Graphics.UI.SDL.flip (screen env)
      event <- waitEventBlocking  
      unless (isEmpty $ imageList env) $  checkEvent event env

main :: IO ()        
main = withInit [InitEverything] $ do
         env <- initEnv
         -- TTFG.init >> loop env
         loop env
