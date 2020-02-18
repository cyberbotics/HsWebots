{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Webots.CameraSpec (main, spec) where

import GHC.Generics
import Data.Int
import Data.VRML
import Data.List
import Test.Hspec
import System.Process
import System.Environment

import Webots.Supervisor
import Webots.Robot
import Webots.Driver
import Webots.Camera
import Webots.Types

import qualified Codec.Picture as I

main :: IO ()
main = do
  args <- getArgs
  case args of
    "controller":_ -> do
      wbu_driver_init
      wbu_driver_set_cruising_speed 10
      wbu_driver_set_steering_angle 0
      camera <- wb_robot_get_device "camera"
      wb_camera_enable camera 50
      camera_width <- wb_camera_get_width camera
      camera_height <- wb_camera_get_height camera
      camera_fov <- wb_camera_get_fov camera
      wb_camera_recognition_enable camera 25
      let loop :: Int -> (Int -> IO ()) -> IO ()
          loop t func = do
            i <- wbu_driver_step
            whenM (i /= -1) $ do
              func t
              loop (t+1) func
      loop 0 $ \t -> do
        i <- wb_robot_get_basic_time_step
        whenM (t==10) $ do
          img <- wb_camera_get_image camera
          I.writePng "pic.png" img
          print "Took a picture"
        return ()
      wbu_driver_cleanup
      
    "supervisor":_ -> do
      withWbRobot $ \time_step -> do
        let loop :: Int -> (Int -> IO ()) -> IO ()
            loop t func = do
              i <- wb_robot_step time_step
              whenM (i /= -1) $ do
                func t
                if t > 100
                then wb_supervisor_simulation_quit(1)
                else loop (t+1) func
        loop 0 $ \i -> do
          return ()


data WorldInfo = WorldInfo
  { northDirection      :: (Float,Float,Float)
  , lineScale           :: Float
  } deriving (Generic,Show,Eq,ToNode)

data Viewpoint = Viewpoint
  { orientation :: (Float,Float,Float,Float)
  , position    :: (Float,Float,Float)
  , near        :: Float
  } deriving (Generic,Show,Eq,ToNode)

viewpoint = Viewpoint
  { orientation = (-1,0,0,1.57)
  , position    = (0,171.0768261655035,0.07181023813347440)
  , near        = 3.0
  }

data Floor = Floor
  { translation :: (Float,Float,Float)
  , size        :: (Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data Recognition = Recognition
  deriving (Generic,Show,Eq,ToNode)

data Camera = Camera
  { width :: Int32
  , height :: Int32
  , recognition :: Maybe Recognition
  } deriving (Generic,Show,Eq,ToNode)

data BmwX5 = BmwX5
  { translation :: (Float,Float,Float)
  , rotation    :: (Float,Float,Float,Float)
  , name        :: String
  , color       :: Color
  , controller  :: String
  , controllerArgs  :: String
  , sensorsSlotFront :: [Node]
  } deriving (Generic,Show,Eq,ToNode)

data Robot = Robot
  { controller :: String
  , controllerArgs  :: String
  , supervisor :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data TexturedBackground = TexturedBackground
  deriving (Generic,Show,Eq,ToNode)
data TexturedBackgroundLight = TexturedBackgroundLight
  deriving (Generic,Show,Eq,ToNode)

genWbt :: String -> IO String
genWbt file = do
  let wbtfile = "test/worlds/test.wbt"
      world =
        VRML
        { version    = "VRML_SIM R2020b utf8"
        , statements =
          [ toNode $ WorldInfo
                     { northDirection      = (0,0,1.0)
                     , lineScale           = 0
                     }
          , toNode $ Viewpoint
                     { orientation = (-1,0,0,1.57)
                     , position    = (0,171.0768261655035,0.07181023813347440)
                     , near        = 3.0
                     }
          , toNode $ Floor
                     { translation = (0.0, -1.0e-3, 0.0)
                     , size       = (1000.0, 1000.0)
                     }
          , toNode TexturedBackground
          , toNode TexturedBackgroundLight
          , toNode $ Robot
                     { controller = "test"
                     , controllerArgs = "../../" ++ file ++ " supervisor"
                     , supervisor = True
                     }
          , toNode $ BmwX5
                     { translation = (0,0,0)
                     , rotation    = (0,0,0,0)
                     , name        = "car"
                     , color       = Color (1,0,0)
                     , controller  = "test"
                     , controllerArgs  = "../../" ++ file ++ " controller"
                     , sensorsSlotFront = [
                         toNode $ Camera
                         { width = 320
                         , height = 240
                         , recognition = Just Recognition
                         }
                         ]
                     }
          ]
        }
  writeVRML wbtfile world
  return wbtfile

run wbt = do
  readProcessWithExitCode
    "webots"
    [ "--mode=fast"
    , "--batch"
    , "--minimize"
    , "--stdout"
    , "--stderr"
    , wbt
    ]
    ""

spec :: Spec
spec = do
  describe "Camera" $ do
    it "capture something" $ do
      wbt <- genWbt "Webots/CameraSpec.hs"
      (code,out,err) <- run wbt
      let outlines = map read $ map (drop 7) $ filter (isPrefixOf "[test] ") $ lines out
      outlines `shouldBe` ["Took a picture"]
