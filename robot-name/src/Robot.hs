module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as MIC
import Control.Monad.State (StateT)
import qualified Control.Monad.State as MS
import Data.IORef (IORef)
import qualified Data.IORef as IOR
import Data.Set (Set)
import qualified Data.Set as S
import qualified System.Random as R

newtype Robot = Robot (IORef String)

newtype RunState = RunState (Set String)

initialState :: RunState
initialState = RunState S.empty

mkRobotName :: StateT RunState IO String
mkRobotName = do
  s <- MS.lift $ M.replicateM 2 $ R.randomRIO ('A', 'Z')
  d <- MS.lift $ M.replicateM 3 $ R.randomRIO ('0', '9')
  let name = s ++ d
  RunState existingNames <- MS.get
  if S.member name existingNames
    then mkRobotName
    else do
      MS.put $ RunState $ S.insert name existingNames
      return name

mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- mkRobotName
  MIC.liftIO $ Robot <$> IOR.newIORef name

resetName :: Robot -> StateT RunState IO ()
resetName (Robot robot) = do
  name <- mkRobotName
  MIC.liftIO $ IOR.writeIORef robot name

robotName :: Robot -> IO String
robotName (Robot robot) = IOR.readIORef robot
