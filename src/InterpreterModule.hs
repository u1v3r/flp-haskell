----------------------------------------------------------
--   Názov projektu: flp-basic
--   Autori: 
--           * Radovan Dvorský - xdvors08 (veduci)
--           * Ondřej Hanzlík - xhanzl05
--           * Martin Klement - xkleme07
--           * Radim Reš - xresra00         
----------------------------------------------------------
-- | Stara sa o vyhodnocovanie naparsovanych vyrazov
module InterpreterModule where

import VariablesModule
import CommonModule

import System.Exit

-- | Vypocty nad premennyma
evaluate :: SymbolTable -> Expr -> Expr
evaluate _ (LiteralInt i) = LiteralInt i
evaluate _ (LiteralDouble d) = LiteralDouble d
evaluate _ (LiteralString s) = LiteralString s
evaluate _ (LiteralBool s) = LiteralBool s
evaluate _ Blank = Blank
evaluate ts (Var v) = fst $ get ts v
evaluate ts (Mult e1 e2) = evaluate ts e1 * evaluate ts e2
evaluate ts (Plus e1 e2) = evaluate ts e1 + evaluate ts e2
evaluate ts (Minus e1 e2) = evaluate ts e1 - evaluate ts e2
evaluate ts (Div e1 e2) = myDiv (evaluate ts e1) (evaluate ts e2)
evaluate ts (UnMinus e) = (UnMinus e)
evaluate ts (Function name params) = getEvaluatedRetValue ts name
evaluate _ _ = error "unknown evaluate operation"

-- | Hromadne vyhodnoti vsetky parametre
evaluateParams :: SymbolTable -> [Expr] -> [Expr]
evaluateParams ts [] = []
evaluateParams ts (param:params) = evaluate ts param : evaluateParams ts params
   
-- | Vyhodnotene True|False vyrazov    
decide :: SymbolTable -> Expr -> Bool
decide ts (Equals e1 e2) = evaluate ts e1 == evaluate ts e2
decide ts (LiteralBool a)
        | a == True = True
        | otherwise = False
decide ts (LiteralInt a)
        | a == 0 = False
        | otherwise = True  
decide ts (Var a) = decide ts (evaluate ts (Var a))
decide _ (Minus (LiteralDouble _) _) = error "bad if condition type"
decide _ (Minus _ (LiteralDouble _)) = error "bad if condition type"
decide _ (Minus (LiteralString _) _) = error "bad if condition type"
decide _ (Minus _ (LiteralString _)) = error "bad if condition type"
decide ts (Minus e1 e2)
        | result == 0 = False
        | otherwise = True
        where result = (evaluate ts e1) - (evaluate ts e2)
decide _ (Plus (LiteralDouble _) _) = error "bad if condition type"
decide _ (Plus _ (LiteralDouble _)) = error "bad if condition type"
decide _ (Plus (LiteralString _) _) = error "bad if condition type"
decide _ (Plus _ (LiteralString _)) = error "bad if condition type"
decide ts (Plus e1 e2)
        | result == 0 = False
        | otherwise = True
        where result = (evaluate ts e1) + (evaluate ts e2)
decide _ (Mult (LiteralDouble _) _) = error "bad if condition type"
decide _ (Mult _ (LiteralDouble _)) = error "bad if condition type"
decide _ (Mult (LiteralString _) _) = error "bad if condition type"
decide _ (Mult _ (LiteralString _)) = error "bad if condition type"
decide ts (Mult e1 e2)
        | result == 0 = False
        | otherwise = True
        where result = (evaluate ts e1) * (evaluate ts e2)
decide ts (NotEquals e1 e2) = evaluate ts e1 /= evaluate ts e2
decide ts (GrT e1 e2 ) = evaluate ts e1 > evaluate ts e2
decide ts (LeT e1 e2 ) = evaluate ts e1 < evaluate ts e2
decide ts (GrTE e1 e2 ) = evaluate ts e1 >= evaluate ts e2
decide ts (LeTE e1 e2 ) = evaluate ts e1 <= evaluate ts e2
decide ts _ = error "bad if condition type"

-- | Inicializuje symboltable na default hodnoty
initValues :: SymbolTable -> IO SymbolTable
initValues [] = return []
initValues ts@(t@(name,value,varType):(others)) = 
              if isVarName value
              then error "variable to variable definition error"
              else
                  do              
                    table <- interpret ts (NewVariable name value varType) 
                    tables <- initValues others           
                    return $ take 1 table ++ tables

-- | Interpretuje pole deklaracii a definicii funkcii
interpretFnc :: SymbolTable -> [Command] -> IO SymbolTable
interpretFnc _ [] = return []
interpretFnc ts (fnc:fncs) = do        
        table <- interpret ts fnc
        tables <- interpretFnc table fncs
        return $ take 1 table ++ tables

-- | Interpretuje vsetky Command prikazy   
interpret :: SymbolTable -> Command -> IO SymbolTable

interpret ts (Document fncs seq) = do--postupne ulozi vsetky deklaracie, definicie a vykonna telo
        fncTs <- interpretFnc ts fncs
        ts' <- interpret fncTs (Seq seq)
        return ts'
        
interpret ts Empty = return ts

interpret ts (NewVariable name value varType) = 
        if value == Blank
        then return $ set ts name initValue varType
        else --ak uz pri deklaracii priradujeme hodnotu treba skontrolovat aj typy             
             assignVar (set ts name Blank varType) name value   
        where initValue
                | varType == IntVar = LiteralInt 0
                | varType == DoubleVar = LiteralDouble 0.0
                | varType == StringVar = LiteralString ""
                | otherwise = Blank

--intrepret ts (Assign v (Var v2)) =
interpret ts (Assign v f@(Function name params)) = do
        ts' <- interpret ts (CallFunction name params)
        assignVar ts' v (evaluate ts' f) 
interpret ts (Assign v e) = assignVar ts v (evaluate ts e)
interpret ts (Print f@(Function name params)) = do
        ts' <- interpret ts (CallFunction name params)
        putStr $ show $ evaluate ts' f
        return ts'
interpret ts (Print v) = do
        putStr $ show $ evaluate ts v
        return ts
interpret ts (Seq []) = return ts
interpret ts (Seq (c:cs)) = 
        if isDefinitionMissing ts--kontrola ci su vsetky funkcie, ktore su deklarovane aj definovane
        then error "missing definition for declared function"
        else do
                ts' <- interpret ts c
                interpret ts' (Seq cs)

interpret ts (SimpleIf b c1) =
        if decide ts b
        then interpret ts c1
        else interpret ts Empty 
        
interpret ts (If b c1 c2) =
       if decide ts b
                then interpret ts c1
                else interpret ts c2
interpret ts (While b c) = 
        if decide ts b
                then do
                       ts' <- interpret ts c
                       interpret ts' (While b c)
                else return ts
interpret ts (Input v) = do
        putStr "\n"--inak nezobrazi text
        val <- readLn :: IO Expr
        assignVar ts v val; 

-- | Funkcia vyhodnoti funkciu a do SymbolTable vlozi vyslednu return hodnotu
interpret ts (CallFunction name params) = do         
        --najskor kontorla ci je uz funkcia definovana alebo deklarovana
        --ak sedia, tak pokracuj, inak chyba
        initVars <- initValues (getFncVariables ts name)--incializacia a kontrola lokalnych premennych        
        initParams <- assignParams (getFncParams ts name) (evaluateParams ts params)--inicializacia a kontrola parametrov funkcie
        fncTs <- interpret (functions++initParams++initVars) (Seq (getFncBody ts name))--interpretacia tela funkcie        
        return $ setFncEvValue ts name (evaluate fncTs (getFncRetValue ts fncTs name))--nastavi return hodnotu do funkcie    
        --(name,fst $ get ts name,snd $ get ts name)   
        where functions = getAllFnc ts--treba predat aj vsetky funkcie ak by funkcia volala funkciu

--vykonna deklaraciu funkcie
interpret ts (DeclareFunction fnc@(FunctionDec name _ _)) =
        if isFncDec ts name --vzdy musi existovat len jedna deklaracia
        then error "Function is already declared"
        else return $ set ts name fnc FunctionDecVar

    
-- | Definicia funkcie a skontroluje vnutrne volania funkcie
interpret ts (DefineFunction fnc@(FunctionDef name params retType body variables retValue _)) =
        if isFncDef ts name 
        then error "Function is already defined"
        else     
                if isFncDec ts name--funkcia je aj deklarovana, treba skontrolovat zhodu parametrov
                then if fncDecAndDefMatch ts name fnc --dec a def sa zhoduje
                     then if length (getFncCalls body) > 0--obsahuje vnutorne volania funkcii
                          then checkFunctionsCall
                         else return $ set ts name fnc FunctionDefVar--funkcia neobsahuje ziadne vnutorne volanie funkcii, nie je treba kontrolovat
                     else error "function declaration a definition doesn't match"
                else checkFunctionsCall--treba skontrolovat ci existuje definicia a tu pouzit na kontorlu, inak chyba
        where checkFunctionsCall = do                              
                                     initVars <- initValues variables --kontrola deklaracii a definicie lokalnych premennych funkcie                                  
                                     --priradi a skontroluje priradenie hodnot do paremetrov volania funkcie
                                     assigned <- assignFncCallsParams (ts++initVars++params) (getFncCalls body)
                                     --treba skontrolovat navratovu hodnotu funkcie
                                     checkReturnType (ts++initVars++assigned) fnc
                                     return $ set ts name fnc FunctionDefVar
                                     
--tu sa rozbali sekvencia deklaracii   
interpret ts (DeclareFncs []) = return ts
interpret ts (DeclareFncs (f:fs)) = do
        ts' <- interpret ts f
        interpret ts' (DeclareFncs fs)

--tu sa rozbali sekvencia definicii
interpret ts (DefineFncs []) = return ts
interpret ts (DefineFncs (f:fs)) = do
        ts' <- interpret ts f
        interpret ts' (DefineFncs fs)

--interpret ts e = error "unknown function interpreter call" 

interpret _ ReturnInScope = exitSuccess

interpret ts _ = return ts


-- | Skontroluje zhodu hodnoty a navratovej hodnoty funkcie
checkReturnType :: SymbolTable -> Expr -> IO SymbolTable
checkReturnType ts (FunctionDef name _ retType _ _ retValue _) = 
        if isExpr retValue
        then    return ts--ake je to vyraz s operaciou tak runtime kontrola
        else
                if isVarName evaluatedRetValue --ake je navratova hodnota premenna
                then case getVarType ts (toString evaluatedRetValue) of
                        Nothing -> error "checkReturnType: variable not found"
                        Just value -> if value == retType || (retType == DoubleVar && value == IntVar)--zhodne typy, alebo return double a hodnota int 
                                      then return ts
                                      else error "function return type doesn't match"
                else if getValueType evaluatedRetValue == BlankVar
                     then if retType == getVariableType || (retType == DoubleVar && getVariableType == IntVar)
                          then return ts
                          else error "function return type doesn't match" 
                     else
                        if retType == getValueType evaluatedRetValue || (retType == DoubleVar && (getValueType evaluatedRetValue) == IntVar)
                        then return ts
                        else error "function return type doesn't match"
                where toString (Var a) = a
                      evaluatedRetValue = evaluate ts retValue
                      getVariableType = snd $ get ts (toString retValue)
              
        
checkReturnType _ _ = error "checkReturnType: unsupported type"

-- | Priradi a skontroluje parametry volanych funkcii
assignFncCallsParams :: SymbolTable -> [Command] -> IO SymbolTable
assignFncCallsParams _ [] = return []
assignFncCallsParams ts (call@(CallFunction name params):(calls)) = do         
          table <- assignParams getParams (evaluateParams ts params)
          tables <- assignFncCallsParams ts calls
          return $ table ++ tables
          where getParams 
                        | isFncDec ts name = getFncDecParams ts name--ak je deklaracia, pouzije deklaraciu
                        | isFncDef ts name = getFncParams ts name--ak je defnicia, pouzije deiniciu
                        | otherwise = error "function declaration or definition not found"

-- | Hromadne priradi vsetky parametre
assignParams :: SymbolTable -> [Expr] -> IO SymbolTable
assignParams [] [] = return []
assignParams ts@(t@(name,value,varType):(others)) params@(cParam:cParams) = 
        if length params /= length ts
        then error "params count doesn't match"
        else do
                table <- assignVar ts name cParam
                tables <- assignParams others cParams
                return $ take 1 table ++ tables

-- | Priradi hodnotu do premennej
assignVar :: SymbolTable -> String -> Expr -> IO SymbolTable
assignVar ts name expr = 
        case getVarType ts name of
                Nothing -> error "Var type not found"
                Just value -> do{
                        if isUnMinusLiteral expr
                        then return $ set ts name ((fst (get ts name)) - (evaluate ts (toValue expr))) value
                        else  
                                if isExpr expr
                                then if value == DoubleVar
                                     then return $ set ts name (castIntToDouble (evaluate ts expr)) value
                                     else return $ set ts name (evaluate ts expr) value
                                else                      
                                        -- typy sa zhoduju alebo je este prazdna hodnota                 
                                        if getValueType expr == value || getValueType expr == BlankVar
                                        then return $ set ts name (evaluate ts expr) value
                                        --podla zadania ak je premena typu Double a prava Int, tak pretypuj, rovnako expr moze byt aj vyraz
                                        else if getValueType expr == IntVar  && value == DoubleVar
                                             then return $ set ts name (evaluate ts $ castIntToDouble expr) value                             
                                             else error "Variable type doesn't match"
                                             }      
                where toValue (UnMinus a) = a
