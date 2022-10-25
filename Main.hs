{-# LANGUAGE UnicodeSyntax #-}
module Main where
import System.IO
import System.Console.ANSI
import Eval
import Parser
import Ast
import Macros
import Data.String

asciiart :: IO ()
asciiart = do
  putStrLn "⠀⠀⠀⢀⣠⣴⣶⣤⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⣴⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠘⣿⣿⣿⣿⡟⠉⢿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠈⠛⠛⠋⠀⠀⠈⣿⣿⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⠀⠀⠀⠀⠀⣸⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⣿⣿⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⠀⢀⣼⣿⣿⣿⣿⡿⢿⣿⣿⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀"
  putStrLn "⠀⠀⠀⢠⣾⣿⣿⣿⣿⡟⠁⠘⣿⣿⣿⣿⣷⠀⠀⠀⣀⡀⠀⠀"
  putStrLn "⠀⠀⣠⣿⣿⣿⣿⣿⠏⠀⠀⠀⢻⣿⣿⣿⣿⡆⣰⣿⣿⣿⣷⡀"
  putStrLn "⠀⣴⣿⣿⣿⣿⣿⠋⠀⠀⠀⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠁"
  putStrLn "⠰⣿⣿⣿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣿⣿⣿⡟⠁⠀"
  putStrLn "⠀⠙⠻⠿⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠿⠟⠛⠁⠀⠀⠀"
  hFlush stdout


namedMode = 0
deBrujinMode = 1




main :: IO ()
main = do
    setSGR [SetColor Foreground Vivid Magenta]
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering
    asciiart
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "                 Juλn Domandl-2022 (CALP)"
    hFlush stdout;
    setSGR [Reset]
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "                 Evaluation: Call by value \n \n"
    hFlush stdout;
    setSGR [Reset]

    loop deBrujinMode macros


namedPrompt = do {
  setSGR [SetColor Foreground Dull Green];
  putStr "Juλn (";
  setSGR [SetColor Foreground Dull Yellow];
  putStr "named 🐇";
  setSGR [SetColor Foreground Dull Green];
  putStr ") ⟹  ";
  setSGR [Reset]
}

deBrujinPrompt = do {
  setSGR [SetColor Foreground Dull Green];
  putStr "Juλn (";
  setSGR [SetColor Foreground Dull Cyan];
  putStr "de Bruijn 🌎";
  setSGR [SetColor Foreground Dull Green];
  putStr ") ⟹  ";
  setSGR [Reset]
}


tryMacro str =
  case getMacro str of
    Just macro -> return macro
    Nothing -> do {printError "Invalid macro!"; return ("","")}

--loop :: IO ()
loop mode macros = do {
    
    if mode == namedMode then
      namedPrompt;
    else
      deBrujinPrompt;

    str' <- getLine;
    str <- pure $ applyMacros str' macros;

    if str == "set de brujin" then
      loop deBrujinMode macros
    else if str == "set named" then
      loop namedMode macros
    else if elem "let" (words str) then
      do {
        macro <- tryMacro str;
        if macro /= ("","") then
          loop mode (macro:macros)
        else
          loop mode macros
      }
    else if mode == deBrujinMode then
        printTasksDeBrujin macros $ tasksDeBrujin str;
    else
      printTasks $ tasks str;
    
    setSGR [Reset];
    loop mode macros
}




printError msg = do {
  setSGR [SetColor Foreground Dull Red];
  putStrLn msg;
  hFlush stdout;
  setSGR [Reset]
}

printSucces msg =  do {
  setSGR [SetColor Foreground Vivid Cyan];
  putStrLn $ msg;
  hFlush stdout;
  setSGR [Reset]
}




tasks str = do {
  e <- parsing str;
  t <- deleteNames [] e;                 
  let 
    t' = eval [] t
    t'' = restoreNames [] t'
  in
    return t''; 
}

tasksDeBrujin str = do {
  e <- parsing str;
  t <- deleteNames [] e;                 
  let 
    t' = eval [] t
  in
    return t'; 
}


printTasks t =
  case t of
    Left err -> printError err
    Right v  -> do {
      printSucces $ printPTerm v;
    }



printCoincidence v macros = sequence_ $ ( 
  getCoincidence v macros >>= (\(x,_) -> [printSucces (" ≈ " ++ x)])
  )

printTasksDeBrujin macros t =
  case t of
    Left err -> printError err
    Right v  -> do {
      printSucces $ printTerm v;
      printCoincidence v macros
    }



parsing str =
  case parseLC str of
    Left err -> Left $ "☠️  parsing error" ++ show err
    Right e -> Right e





