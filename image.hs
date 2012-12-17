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
import Control.Monad.State.Lazy

--import Control.Monad.Identity

import Data.CircularList
import Data.Maybe

import ImagePaths (getImages)
import ImageConfig

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

loadAdjustedImage :: StateT Config IO ()
loadAdjustedImage = get >>= \env -> do
  oldImage <- liftIO $ loadImage . fromJust . focus . imageList $ env
  case viewMode env of     
    Fit             -> let ratio = fitImageRatio env{currentImage = oldImage}
                       in (liftIO $ zoom oldImage ratio ratio False) >>= \i -> put env{currentImage = i}
    (Zoom ratio _)  -> put env{currentImage = oldImage, viewMode = Full}           
    _               -> put env{currentImage = oldImage}

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

changeImage :: (CList String -> CList String) -> StateT Config IO ()
changeImage op = get >>= (\env -> return env{imageList = op $ imageList env, offset = (0,0)})
                 >> loadAdjustedImage 

resize :: Int -> Int -> StateT Config IO ()
resize w h = do env <- get 
                s <- liftIO $ setVideoMode w h screenBpp [Resizable] 
                put env{windowW = w, windowH = h, screen  = s}

fitImage :: StateT Config IO ()
fitImage = do env <- get
              let ratio = fitImageRatio env
              i <- liftIO $ zoom (currentImage env) ratio ratio False
              put env{viewMode = Fit, currentImage = i, offset = (0,0) } 

fullImage :: StateT Config IO ()
fullImage = get >>= \env -> (do i <- liftIO $ loadImage (fromJust . focus . imageList $ env)
                                put env{viewMode = Full, currentImage = i})

zoomWith :: (Double -> Double -> Double) -> StateT Config IO ()
zoomWith op = do env <- get
                 case env of
                   Config {imageList=images, viewMode=(Zoom ratio origImage)}
                       -> do let newRatio = ratio `op` zoomStep
                             newImage <- liftIO $ zoom origImage newRatio newRatio False
                             put env{viewMode = Zoom newRatio origImage, currentImage = newImage}
                   _   -> put env{viewMode = Zoom 1 (currentImage env)} >> zoomWith op 

moveHor :: (Int -> Int -> Int) -> StateT Config IO ()
moveHor op = do env <- get
                let oldOffset = offset env
                let newOffset = (fst oldOffset `op` moveStep, snd oldOffset)
                put env{offset = newOffset} 

moveVer :: (Int -> Int -> Int) -> StateT Config IO ()
moveVer op = get >>= \env -> let oldOffset = offset env 
                             in put env{offset = (fst oldOffset, snd oldOffset `op` moveStep)} 

-- switchInfo :: Config -> IO Config
-- switchInfo env = return env{infoText = not $ infoText env}

checkEvent :: Event -> StateT Config IO ()
checkEvent event = do
  env <- get
  case event of 
    (KeyDown (Keysym key _ _)) -> 
        case key of
          SDLK_ESCAPE -> put env{imageList=empty}
          SDLK_RIGHT  -> changeImage rotR
          SDLK_LEFT   -> changeImage rotL
          SDLK_f      -> fitImage
          SDLK_v      -> fullImage
          SDLK_i      -> zoomWith (+)
          SDLK_o      -> zoomWith (-)
          SDLK_a      -> moveHor (-) -- move left
          SDLK_d      -> moveHor (+) -- move right
          SDLK_w      -> moveVer (-) -- move up
          SDLK_s      -> moveVer (+) -- move down
                 --                                       SDLK_t      -> switchInfo env
          _           ->  return ()
    (VideoResize width height) -> resize width height >> fitImage
    _                          -> return ()
                   
initEnv :: IO Config
initEnv = do
  args      <- getArgs
  screen    <- setVideoMode windowWidth windowheight screenBpp [Resizable]
  imageList <- liftM (fromList . concat) $ mapM getImages args
  image     <- loadImage $ fromJust $ focus imageList
  let windowW = surfaceGetWidth screen
      windowH = surfaceGetHeight screen
  return (Config screen imageList image windowW windowH Full (0,0) False)

loop :: ConfEnv
loop = do
      env <- get
      -- Clear screen
      liftIO $ fillRect (screen env) Nothing (Pixel 0)
      liftIO $ uncurry applySurface (offset env) (currentImage env) (screen env)
      
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

      liftIO $ Graphics.UI.SDL.flip $ screen env 
      event <- liftIO waitEventBlocking  
      if not (isEmpty $ imageList env) 
      then checkEvent event >> loop
      else return env

main :: IO (Config, Config)        
main = withInit [InitEverything] $ do
         env <- initEnv
       -- TTFG.init >> loop env
         runStateT loop env
