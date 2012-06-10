----------------------------------------------------------
--   Názov projektu: flp-basic
--   Autori: 
--           * Radovan Dvorský - xdvors08 (veduci)
--           * Ondřej Hanzlík - xhanzl05
--           * Martin Klement - xkleme07
--           * Radim Reš - xresra00         
----------------------------------------------------------
-- | Obsahuje spolocne funkcie pre vsetky moduly
module CommonModule where

import Data.Char

-- | Odstranenie viacriadkovych komentarov
removeLongComments :: String -> String
removeLongComments [] = []
removeLongComments [x] = [x]
removeLongComments (x:y:xs) = 
        if (x == '/' && y == '\'')
        then insideLongComments xs
        else x : removeLongComments (y:xs)

insideLongComments :: String -> String
insideLongComments [] = []
insideLongComments [x] = [x]
insideLongComments (x:y:xs) = 
        if (x == '\'' && y == '/')
        then removeLongComments xs
        else insideLongComments (y:xs)
        

-- | Odstranenie riadkovych komentarov
removeLineComments :: String -> String
removeLineComments [] = []
removeLineComments [x] = [x]
removeLineComments (x:xs) = 
        if x == '\''
        then insideLineComments (xs)
        else x : removeLineComments (xs)
        
insideLineComments :: String -> String
insideLineComments [] = []
insideLineComments [x] = [x]
insideLineComments (x:xs) = 
        if x == '\n'
        then x : removeLineComments xs
        else insideLineComments xs
            
-- | Nahradi vsetky vyskyty char za string
replace :: String -> Char -> String -> String
replace [] _ _ = []
replace (x:xs) char repl = 
        if x==char 
        then repl ++ replace xs char repl
        else x : replace xs char repl

-- | Vsetky pismena male
toLowerCase :: String -> String
toLowerCase string = [ toLower x | x<-string]

fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c         


replaceString :: Eq a => [a] -> [a] -> [a] -> [a]
replaceString [] _ _ = []
replaceString s find repl =
    if take (length find) s == find
        then repl ++ (replaceString (drop (length find) s) find repl)
        else [head s] ++ (replaceString (tail s) find repl)

-- | Nahradi vsetky prikazy return v scope za __return_in_scope__
replaceReturnInScope :: String -> String
replaceReturnInScope [] = []
replaceReturnInScope [x] = [x]
replaceReturnInScope (x:y:z:u:v:xs)  = 
        if (x == 's' &&  y == 'c' && z == 'o' && u == 'p' && v == 'e')
        then x: replaceString (y:z:u:v:xs) "return" "__return_in_scope__"
        else x : replaceReturnInScope (y:z:u:v:xs)
replaceReturnInScope (x:xs) = x : replaceReturnInScope xs                