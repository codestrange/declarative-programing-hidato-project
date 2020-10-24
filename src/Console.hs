{-# LANGUAGE BlockArguments #-}

module Console
( start
, commandHelp
, consoleLoop
, commandExit
, Command (..),
) where


import Control.Monad (when)
import Data.List (find)
import Debug.Trace
import System.IO


start :: [Command] -> IO ()
start cmds = do
  let cmdList = cmds ++ [commandExit]
  let cmdListWithHelp = commandHelp (createHelp : cmdList) : cmdList
  consoleLoop cmdListWithHelp
  where 
    createHelp = commandHelp [] 


consoleLoop :: [Command] -> IO ()
consoleLoop cmds = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let command = words input
  when (null command) do
    -- empty case, just enter
    start cmds
  let r = find (\a -> head command == cmdName a) cmds
  case r of
    Just c -> do
      ret <- cmdIO c $ tail command
      case ret of
        0 -> consoleLoop cmds
        1 -> return ()
    Nothing -> do 
      putStrLn $ "Command \"" ++ head command ++ "\" not found. Try \"help\" to find all avalidable commands."
      consoleLoop cmds


data Command = Command
  { cmdName :: String,
    cmdDescription :: String,
    cmdArgs :: [String],
    cmdIO :: [String] -> IO Int
  }


-- Help Commands
commandHelp :: [Command] -> Command
commandHelp cmds =
  Command
    { cmdName = "help",
      cmdDescription = "The Help command will help you in all your needs",
      cmdArgs = ["COMMAND"],
      cmdIO = \args -> do
        case args of
          [] -> do
            putStrLn "Avalidable Commands: "
            putStr $ unlines $ map printCmd cmds
            putStrLn ""
          [command] -> do
            let r = find (\a -> command == cmdName a) cmds
            case r of
              Just c ->
                putStr $
                  unlines
                    [ cmdName c ++ " - " ++ cmdDescription c,
                      "Usage: " ++ cmdName c ++ " " ++ printArgs (cmdArgs c),
                      ""
                    ]
              Nothing -> putStrLn $ "Command \"" ++ command ++ "\" not found. Try \"help\" to find all avalidable commands."
        return 0
    }
  where
    printArgs args = foldl (\a b -> a ++ "[" ++ b ++ "] ") "" args
    printCmd c = unwords ["\t", cmdName c, "\t\t", printArgs (cmdArgs c)]


commandExit :: Command
commandExit =
  Command
    { cmdName = "exit",
      cmdDescription = "Exit the console",
      cmdArgs = [],
      cmdIO = \_ -> do
        putStrLn "Are you sure? y/n"
        answer <- getLine
        return case answer of
          "y" -> 1
          "n" -> 0
    }
