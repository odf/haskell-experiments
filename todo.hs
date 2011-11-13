-- Example todo program from "Learn you a Haskell [...]", slightly modified.

import System.Environment   
import System.Directory
import System.IO
import Data.List
  
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  

main = do  
  (command:args) <- getArgs  
  let (Just action) = lookup command dispatch  
  action args  

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  

view :: [String] -> IO ()  
view [fileName] = do  
  contents <- readFile fileName  
  let numberedTasks = zip [0..] $ lines contents
  putStr $ unlines $ map (\(n, s) -> show n ++ " - " ++ s) numberedTasks  
  
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
  handle <- openFile fileName ReadMode  
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle  
  let number = read numberString  
      todoTasks = lines contents  
      newTodoItems = delete (todoTasks !! number) todoTasks  
  hPutStr tempHandle $ unlines newTodoItems  
  hClose handle  
  hClose tempHandle  
  removeFile fileName  
  renameFile tempName fileName    
