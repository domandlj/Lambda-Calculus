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



data Mode = Named | Unnamed deriving Eq





main :: IO ()
main = do
    setSGR [SetColor Foreground Vivid Magenta]
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering
    asciiart
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "                 Juλn Domandl-2022 (FAMAF-CALP)"
    hFlush stdout;
    setSGR [Reset]
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "                 Untyped lambda calculus interpreter"
    putStrLn "                 Evaluation: call-by-value \n \n"
    hFlush stdout;
    setSGR [Reset]

    loop Unnamed macros


namedPrompt = do {
  setSGR [SetColor Foreground Dull Green];
  putStr "MyLamb (";
  setSGR [SetColor Foreground Dull Yellow];
  putStr "named 🐇";
  setSGR [SetColor Foreground Dull Green];
  putStr ") ⟹  ";
  setSGR [Reset]
}

deBrujinPrompt = do {
  setSGR [SetColor Foreground Dull Green];
  putStr "MyLamb (";
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

loop mode macros = do {
    
    if mode == Named then
      namedPrompt;
    else
      deBrujinPrompt;

    str' <- getLine;
    str <- pure $ applyMacros str' macros;

    if str == "set unnamed" then
      loop Unnamed macros
    else if str == "set named" then
      loop Named macros
    else if elem "let" (words str) then
      do {
        macro <- tryMacro str;
        if macro /= ("","") then
          loop mode (macro:macros)
        else
          loop mode macros
      }
    else if mode == Unnamed then
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





