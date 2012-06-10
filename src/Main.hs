----------------------------------------------------------
--   Názov projektu: flp-basic
--   Autori: 
--           * Radovan Dvorský - xdvors08 (veduci)
--           * Ondřej Hanzlík - xhanzl05
--           * Martin Klement - xkleme07
--           * Radim Reš - xresra00         
----------------------------------------------------------
-- | Zakladny modul programu
module Main where

import InterpreterModule
import CommonModule
import ParserModule
import System.Environment
  
-- | Main programu     
main = do
        args <- getArgs
        if length args /= 1
                then error "just one argument!"
                else do
                        let fileName = head args
                        input <- readFile fileName
                        --kazdy novy riadok sa nahradi znackou ${newline} a rovnako je treba odstranit riadkove komentar inak sa to bije
                        let ast = parseAep (parsedInput input) fileName  
                        --    fncs = parseFunctions (parsedInput input) fileName                      
                        --interpretFnc [] fncs--vyhodnocuje funkcie
                        interpret [] ast--postara sa o vyhodnotenie zakladnych prikazov a struktur
                        where parsedInput input = 
                                toLowerCase (replace (replace (replaceReturnInScope $ removeLineComments $ removeLongComments input++"\n") '\n' "${newline}") '\r' "")

--test =  do
--         input <- readFile "/home/rdy/haskell/workspace/flp-basic/myTests/basic.test"
--         putStrLn $ toLowerCase (replace (replace (replaceReturnInScope $ removeLineComments $ removeLongComments input++"\n") '\n' "${newline}") '\r' "${newLine}")
--             