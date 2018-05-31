import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, task] = appendFile fileName (task ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let tasks = lines contents
      numberedTasks = zipWith (\n task -> show n ++ "-" ++ task) [0..] tasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, index] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read index
      tasks = lines contents
      newTasks = delete (tasks !! number) tasks
  hPutStr tempHandle $ unlines newTasks
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, index] = do
  contents <- readFile fileName
  (tempName, tempHandle) <- openTempFile "." "temp"
  let tasks = lines contents
      number = read index
      taskToBump = tasks !! number
      tasksWithoutBump = delete taskToBump tasks
      newTasks = (taskToBump:tasksWithoutBump)
  hPutStr tempHandle $ unlines newTasks
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
