module Main where

{- 
   * Add error checking:
   ** Loading files -> stop with meaningful error if no image loaded
   * Restriced movement space in moveHor and moveVer
-}

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import System.Environment (getArgs)
import System.FilePath (takeFileName)

import Control.Monad (mapM, unless)
import Control.Applicative ((<$>))
import Control.Monad.State.Lazy

import Control.Arrow (first,second)

--import Control.Monad.Identity

import Data.CircularList
import Data.Maybe

import ImagePaths (getImages)
import ImageConfig

{- TODO
   Add proper error handling for fromJust.
-}
loadAdjustedImage :: ConfState
loadAdjustedImage = do
  env <- get
  oldImage <- liftIO $ loadCurrentImage env
  case viewMode env of     
    Fit             -> let ratio = fitImageRatio env{currentImage = oldImage}
                       in liftIO (zoom oldImage ratio ratio False) >>= \i -> put env{currentImage = i}
    (Zoom ratio _)  -> put env{currentImage = oldImage, viewMode = Full}           
    _               -> put env{currentImage = oldImage}

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

-- Fit image
fitImageRatio :: Config -> Double
fitImageRatio env = if ratio1 < ratio2 then ratio1 else ratio2
    where
      intDiv :: Int -> Int -> Double
      intDiv x y = fromIntegral x / fromIntegral y
      image  = currentImage env      
      ratio1 = windowW env `intDiv` surfaceGetWidth image
      ratio2 = windowH env `intDiv` surfaceGetHeight image

fitImage :: ConfState
fitImage = do env <- get
              let ratio = fitImageRatio env
              i   <- liftIO $ zoom (currentImage env) ratio ratio False
              put env{viewMode = Fit, currentImage = i, offset = (0,0)} 

-- changeImage

changeImage :: (CList String -> CList String) -> ConfState
changeImage op = do env <- get
                    liftIO $ Graphics.UI.SDL.freeSurface (currentImage env)
                    liftIO $ Graphics.UI.SDL.freeSurface (screen env)
                    put env{imageList = op $ imageList env, offset = (0,0) } 
                    loadAdjustedImage

resize :: Int -> Int -> ConfState
resize w h = do env <- get 
                s   <- liftIO $ setVideoMode w h screenBpp [Resizable] 
                put env{windowW = w, windowH = h, screen  = s}

fullImage :: ConfState
fullImage = do env <- get
               i   <- liftIO $ loadCurrentImage env
               put env{viewMode = Full, currentImage = i}

zoomWith :: (Double -> Double -> Double) -> ConfState
zoomWith op = do env <- get
                 case env of
                   Config {imageList=images, viewMode=(Zoom ratio origImage)}
                       -> do let newRatio = ratio `op` zoomStep
                             newImage <- liftIO $ zoom origImage newRatio newRatio False
                             put env{viewMode = Zoom newRatio origImage, currentImage = newImage}
                   _   -> put env{viewMode = Zoom 1 (currentImage env)} >> zoomWith op 

moveHor :: (Int -> Int -> Int) -> ConfState
moveHor op = modify (\env -> env{offset = first (`op` moveStep) $ offset env})

moveVer :: (Int -> Int -> Int) -> ConfState
moveVer op = modify (\env -> env{offset = second (`op`moveStep) $ offset env})

-- switchInfo :: IO Config
-- switchInfo = ask >>= \env -> return env{infoText = not $ infoText env}

checkEvent :: Event -> ConfState
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
  imageList <- fromList . concat <$> mapM getImages args
  let windowW = surfaceGetWidth screen
      windowH = surfaceGetHeight screen
  image     <- getImage imageList                    
  return $ Config screen imageList image windowW windowH Full (0,0) False

loop :: ConfState
loop = do
      env <- get
      liftIO (fillRect (screen env) Nothing (Pixel 0)  -- Clear screen
              >> uncurry applySurface (offset env) (currentImage env) (screen env)
              -- Here we will put info text etc.
              >> Graphics.UI.SDL.flip (screen env))
      unless (isEmpty . imageList $ env) 
       (liftIO waitEventBlocking >>= checkEvent >> loop)

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

       
main :: IO ()        
main = withInit [] 
          $ void (initEnv >>= runStateT loop)

-- Helpers

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat 

getImage :: CList String -> IO Surface
getImage = loadImage . imageFromMaybe . focus
  where imageFromMaybe :: Maybe a -> a
        imageFromMaybe (Just i) = i
        imageFromMaybe Nothing  = error "No image loaded" 

loadCurrentImage :: Config -> IO Surface
loadCurrentImage = getImage . imageList

