----------------------------------------------------------
--   Názov projektu: flp-basic
--   Autori: 
--           * Radovan Dvorský - xdvors08 (veduci)
--           * Ondřej Hanzlík - xhanzl05
--           * Martin Klement - xkleme07
--           * Radim Reš - xresra00         
----------------------------------------------------------
-- | Obsahuje funkcie a struktury na pracu s funkciama a premennyma
module VariablesModule where

import Data.Char

import CommonModule
import qualified Data.List as L

-- | Hodnoty premennych
data Expr = Blank
        | LiteralInt Integer
        | LiteralDouble Double
        | LiteralBool Bool
        | LiteralString String
        | Var String            --meno premennej
        | Mult Expr Expr
        | Plus Expr Expr
        | Minus Expr Expr
        | Div Expr Expr
        | GrT Expr Expr
        | LeT Expr Expr
        | Equals Expr Expr
        | NotEquals Expr Expr
        | GrTE Expr Expr
        | LeTE Expr Expr
        | UnMinus Expr   
        | Function String [Expr]--meno,parametre     
        | FunctionDef { 
                       fncName :: String  --meno funkcie
                     , fncParams :: [(String,Expr,VarTypes)] --parametre (meno,hodnota,typ) 
                     , retType :: VarTypes  --typ navratovej hodnoty
                     , body :: [Command]  --prikazy funkcie
                     , fncVariables :: SymbolTable  --lokalne premenne
                     , retValue :: Expr  --navratova hodnota
                     , evRetValue :: Expr --obsahuje vyhodnotenu return hodnotu po vykonani funkcie
                     }
        --deklaracia funkcie - meno funkcie, parametre, typ navratovej hodnoty
        | FunctionDec String [(String,Expr,VarTypes)] VarTypes
        deriving (Eq,Ord)
        
-- | Datova struktura pre prikazy      
data Command = Empty
        | Assign String Expr               --priradenie hodnoty
        | NewVariable String Expr VarTypes --deklaracia typu premennej
        | Print Expr
        | Seq [Command]
        | Input String
        | If Expr Command Command
        | SimpleIf Expr Command
        | While Expr Command         
        | DeclareFunction Expr
        | DeclareFncs [Command]--zoznam vsetkych deklaracii
        | DefineFncs [Command]--zoznamam definicii funkcii
        | DefineFunction Expr
        | ReturnInScope
        | CallFunction String [Expr]--meno funkcie,parameter funkcie = [premenna|hodnota|cokolvek]    
        | Document [Command] [Command] --prenasa cely naparsovany dokument, v tvare: deklaracie + definicie, sekvenicia scope prikazov 
        deriving (Show,Ord,Eq)

       
-- |  List (meno,hodnota,typ) premennych
type SymbolTable = [(String, Expr, VarTypes)]


-- | Definicia vypoctov nad Expr
instance Num Expr where
        LiteralInt a + LiteralInt b = LiteralInt (a + b)
        LiteralInt a + LiteralDouble b = LiteralDouble (fromIntegral a + b)
        LiteralDouble a + LiteralInt b = LiteralDouble (a + fromIntegral b)
        LiteralDouble a + LiteralDouble b = LiteralDouble (a + b)
        LiteralString a + LiteralString b = LiteralString (a++b)
        LiteralInt a + LiteralString b = LiteralString (show a ++ b)
        LiteralString a + LiteralInt b = LiteralString (a ++ show b)
        LiteralDouble a + LiteralString b = LiteralString (show a ++ b)
        LiteralString a + LiteralDouble b = LiteralString (a ++ show b)
        _ + _ = error "unsupported type operation"
        LiteralInt a - LiteralInt b = LiteralInt (a - b)
        LiteralDouble a - LiteralDouble b = LiteralDouble (a - b)
        LiteralInt a - LiteralDouble b = LiteralDouble (fromIntegral a - b)
        LiteralDouble a - LiteralInt b = LiteralDouble (a - fromIntegral b)
        _ - _ = error "unsupported type operation"
        LiteralInt a * LiteralInt b = LiteralInt (a * b)
        LiteralDouble a * LiteralDouble b = LiteralDouble (a * b)
        LiteralInt a * LiteralDouble b = LiteralDouble (fromIntegral a * b)
        LiteralDouble a * LiteralInt b = LiteralDouble (a * fromIntegral b)
        _ * _ = error "unsupported type operation"
        abs (LiteralInt a) = LiteralInt (abs a)
        abs (LiteralDouble a) = LiteralDouble (abs a)
        fromInteger a = LiteralInt a
        signum (LiteralInt x)
                | x < 0 = -1
                | x > 0 = 1
                | x == 0 = 0
        signum (LiteralDouble x)
                | x < 0 = -1
                | x > 0 = 1
                | x == 0 = 0

-- | Zabezpeci spravne zobrazenie na vystup
instance Show Expr where
      show (LiteralInt x) = show x
      show (LiteralDouble x) = show x
      show (LiteralBool x)--podla zadania treba zobrazovat 0 a 1
                | x == True = show 1
                | otherwise = show 0
      show (LiteralString x) = x      
      show (UnMinus x) 
        | isIntLiteral x = show x
        | isDoubleLiteral x = show x
        | otherwise = error "bad unary operator type"
      show (Var x) = x
      show Blank = "Blank" 
      show (Function name params) = "Calling function: " ++ (show name) ++ " with params: " ++ show params
      show (FunctionDef name params retType body vars retValue evRetValue) = "Definicia funkcie"
      show (FunctionDec _ _ _) = "Deklaracia funkcie"
      
-- | Zabezpeci spravne cistanie zo vstupu
instance Read Expr where
        readsPrec _ value = parseTuple $ makeTuple value
                where makeTuple v
                        | isEmpty v = (v,Blank)
                        | isString v = (v,LiteralString (read v :: String))
                        | isInteger v = (v,LiteralInt (read v :: Integer))
                        | isDouble v = (v, LiteralDouble (read v :: Double))
                        | isBool v = (v, LiteralBool (read $ toUpper (head v) : tail v :: Bool))
                        | otherwise = error "Unknown type"
                                where   isEmpty v = if length v == 0 then True else False 
                                        isString string@(x:xs) = 
                                                if head string == '"' && last string == '"' 
                                                then True 
                                                else False   
                                        isBool v = toLowerCase v == "true" || toLowerCase v == "false"
                                        isInteger number@(x:xs) = if x == '+' || x == '-'
                                                                  then isInteger xs
                                                                  else case reads number :: [(Integer, String)] of
                                                                               [(_, "")] -> True
                                                                               _         -> False
                                         
                                        isDouble s = case reads s :: [(Double, String)] of
                                          [(_, "")] -> True
                                          _         -> False
                      parseTuple (attempt, result) = [(result, drop (length attempt) value)]

-- | Pretypuje LiteralInt na LiteralDouble
castIntToDouble :: Expr -> Expr
castIntToDouble (LiteralInt a) = LiteralDouble $ fromIntegral a
castIntToDouble (LiteralDouble a) = LiteralDouble a

-- | Delenie Expr vyrazov
myDiv :: Expr -> Expr -> Expr
myDiv (LiteralInt x) (LiteralInt y) = LiteralDouble (fromIntegral x / fromIntegral y)
myDiv (LiteralDouble x) (LiteralDouble y) = LiteralDouble (x / y)
myDiv (LiteralInt x) (LiteralDouble y) = LiteralDouble(fromIntegral x / y)
myDiv (LiteralDouble x) (LiteralInt y) = LiteralDouble(x / fromIntegral y)


-- | Vlozi premennu resp. funkciu do tabulky
set :: SymbolTable -> String -> Expr -> VarTypes -> SymbolTable
set [] var val varType = [(var, val,varType)]
set ts var val FunctionDefVar = (var, val,FunctionDefVar):ts
set ts var val FunctionDecVar = (var, val,FunctionDecVar):ts
set ts@(s@(v,_,vt):ss) var val varType =
                        if vt == FunctionDefVar || vt == FunctionDecVar
                        then (var,val,varType):ts
                        else
                                if v == var
                                then (var, val, varType):ss
                                else s : set ss var val varType
                     
--set ts@(s@(v,_,_):ss) var val varType =
--        --deklaracia a definicia funkcie moze mat rovnaky nazov
--        if varType == FunctionDefVar || varType == FunctionDecVar
--        then s : ts
--        else--premmene musia mat jedinecne meno
--                if v == var
--                        then (var, val, varType):ss
--                        else s : set ss var val varType



-- | Vyhlada premennu, nevracia funkcie
get :: SymbolTable -> String -> (Expr,VarTypes)                
get [] v = error "variable not found"
get ((var, val, varType):vars) v =
        if v == var && (varType /= FunctionDefVar || varType /= FunctionDecVar)
                then (val,varType)
                else get vars v

-- | Vrati vsetky definicie funkcii
getAllFnc :: SymbolTable -> SymbolTable
getAllFnc [] = []
getAllFnc (t@(var, val, varType):vars) =
        if varType == FunctionDefVar
        then t : getAllFnc vars
        else getAllFnc vars

-- | Vrati vsetky deklaracie funkcii
getAllFncDec :: SymbolTable -> SymbolTable
getAllFncDec [] = []
getAllFncDec (t@(var, val, varType):vars) =
        if varType == FunctionDecVar
        then t : getAllFncDec vars
        else getAllFncDec vars

-- | Vyhlada definiciu funkcie
getFncDef :: SymbolTable -> String -> ([(String,Expr,VarTypes)],VarTypes,[Command],SymbolTable,Expr,Expr)
getFncDef [] _ = error "function definition not found"
getFncDef ((name, FunctionDef _ params retType body fncVariables retValue evValue, varType):functions) search =
        if name == search && varType == FunctionDefVar
                then (params, retType, body, fncVariables, retValue,evValue)
                else getFncDef functions search
getFncDef all@((name, value, varType):vars) search = getFncDef vars search


-- | Vyhlada defniciu funkcie a vrati ju celu ako je ulozena v SymbolTable
getFncDefAll :: SymbolTable -> String -> Expr
getFncDefAll [] _ = error "function definition not found"
getFncDefAll ((name, val, varType):functions) search =
        if name == search && varType == FunctionDefVar
                then val
                else getFncDefAll functions search


-- | Zisti ci uz je funkcia definovana
isFncDef :: SymbolTable -> String -> Bool
isFncDef [] _ = False
isFncDef ((var,_,varType):functions) search = 
                     if var == search && varType == FunctionDefVar 
                     then True
                     else isFncDef functions search

-- | Vyhlada deklaraciu funkcie
getFncDec :: SymbolTable -> String -> Maybe ([(String,Expr,VarTypes)],VarTypes)
getFncDec [] _ = Nothing
getFncDec ((name, FunctionDec _ params retType,varType):functions) search =
        if name == search && varType == FunctionDecVar
                then Just (params, retType)
                else getFncDec functions search
getFncDec all@((name, value, varType):vars) search = getFncDec vars search

-- | Zisti ci je uz funkcia deklarovana
isFncDec :: SymbolTable -> String -> Bool
isFncDec ts search = case getFncDec ts search of
                        Nothing -> False
                        Just _ -> True

-- | Vrati vsetky parametre funkcie
getFncParams :: SymbolTable -> String -> [(String,Expr,VarTypes)]   
getFncParams ts search = params' $ getFncDef ts search
        where params' (params,_,_,_,_,_) = params
        
-- | Vrati typ navratovej hodnoty funkcie
getFncRetType :: SymbolTable -> String -> VarTypes
getFncRetType ts search = retType' $ getFncDef ts search
        where retType' (_,retType,_,_,_,_) = retType
        
-- | Vrati telo funkcie (sekvenciu prikazov)
getFncBody :: SymbolTable -> String -> [Command]
getFncBody ts search = body' $ getFncDef ts search
        where body' (_,_,body,_,_,_) = body

-- | Vsetky lokalne premenne funkcie
getFncVariables :: SymbolTable -> String -> SymbolTable
getFncVariables ts search = fncVariables' $ getFncDef ts search
        where fncVariables' (_,_,_,fncVariables,_,_) = fncVariables

-- | Vrati hodnotu navratovej hodnoty ako Expr bez vyhodnotenia
getFncRetValueExpr :: SymbolTable -> String -> Expr
getFncRetValueExpr ts search = retValue' $ getFncDef ts search
        where retValue' (_,_,_,_,retValue,_) = retValue

-- | Vrati navratovu hodnotu funkcie a ak je to premenna tak aj vyhodnoti jej hodnotu
getFncRetValue :: SymbolTable -> SymbolTable -> String -> Expr
getFncRetValue ts vars search = retValue' $ getFncDef ts search
        where retValue' (_,_,_,_,retValue,_) = 
                if isVarName retValue--ak je to premmena treba vratit hodnotu
                then fst $ get vars (show retValue)
                else retValue--inak vrat literal

-- | Pouziva sa ak je navratova hodnota premenna, tak vrati hodnotu
getEvaluatedRetValue :: SymbolTable -> String -> Expr
getEvaluatedRetValue ts search = fncEvRetValue' $ getFncDef ts search
        where fncEvRetValue' (_,_,_,_,_,evValue) = evValue


-- | Vrati vsetky parametre deklaracia funkcie
getFncDecParams :: SymbolTable -> String -> [(String,Expr,VarTypes)]
getFncDecParams ts search = case getFncDec ts search of
                                Nothing -> error "getFncDecParams: function declaration not found"
                                Just value -> fst value

-- | Vrati typ navratovej hodnoty deklaracie funkcie
getFncDecRetType :: SymbolTable -> String -> VarTypes
getFncDecRetType ts search = case getFncDec ts search of
                                Nothing -> error "getFncDecRetType: function declaration not found"
                                Just value -> snd value

-- | Zo vsetkych prikazov vyfiltruje len volania funkcii
getFncCalls :: [Command] -> [Command]
getFncCalls [] = []
getFncCalls (c:cs) = case c of
                        (CallFunction _ _) -> c : getFncCalls cs--priame volanie funkcie
                        (Assign _ (Function name params)) -> (CallFunction name params) : getFncCalls cs--priradenie funkcie do premennej
                        (Print (Function name params)) -> (CallFunction name params) : getFncCalls cs--volanie pri print
                        _ -> getFncCalls cs--ostatne

-- | Skontroluje typy parametrov a navratovu hodnotu definicii a deklaracii funkcii
fncDecAndDefMatch :: SymbolTable -> String -> Expr -> Bool
fncDecAndDefMatch ts name (FunctionDef _ params retType _ _ _ _) = 
                            if fncDecRetType == retType --kontrola navratoveho typu funkcie
                            then 
                                    if length fncDecParams /= length params--rovnaky pocet parametrov
                                    then errorMsg
                                    else if checkTypes fncDecParams params--typy sa zhoduju
                                         then True
                                         else errorMsg                                    
                            else False
                            where fncDecParams = getFncDecParams ts name
                                  fncDecRetType = getFncDecRetType ts name
                                  errorMsg = error "function declaration a definition doesn't match"
                                  checkTypes [] [] = True
                                  checkTypes (c@(cName,_,cType):decs) (f@(fName,_,fType):defs) = 
                                                if cType /= fType --ak sa typy nerovnaju
                                                then False
                                                else checkTypes decs defs

-- | Nastavi vyhodnotenu hodnotu do funkcie a skontroluje typ navratovej hodnoty a hodnotu
setFncEvValue :: SymbolTable -> String -> Expr -> SymbolTable
setFncEvValue ts search evValue = do        
        set deletedTable search newFunction FunctionDefVar--vytvor novu s vyhodnotenou hodnotou
        where newFunction = FunctionDef search params retType body fncVariables retValue evValueChecked              
                where params = getFncParams ts search--parametre
                      retType = getFncRetType ts search--typ navrahovej hodnoty
                      body = getFncBody ts search--telo funkcie
                      fncVariables = getFncVariables ts search--lokalne premenne funkcie
                      retValue = getFncRetValueExpr ts search--vrati nevyhodnotenu Expr navratovu hodnotu
                      evValueChecked =
                                if getValueType evValue == retType --ak sa zhoduju navratove typy tak vrat
                                then evValue
                                else if retType == DoubleVar && getValueType evValue == IntVar
                                     --ak je navratovy typ funkcie double a hodnota int, tak pretypuj na double, inak chyba
                                     then castIntToDouble evValue
                                     else error "function return type doesnt'match"
              deletedTable = L.delete (search,getFncDefAll ts search,FunctionDefVar) ts--odstran staru fnc bez vyhodnotenej hodnoty

-- | Skontroluje ci nechyba definicie pre niektoru deklraciu
isDefinitionMissing :: SymbolTable -> Bool
isDefinitionMissing ts = checkDec (getAllFncDec ts)
        where checkDec [] = False              
              checkDec ((_,(FunctionDec name _ _),_):(decs)) =
                                if isFncDef ts name == False--nie je definovane
                                then True
                                else checkDec decs
              checkDec (ts:others) = checkDec others
              
        
              
-- | Typy premennych pouzivanych pri typovej kontrole
data VarTypes = BoolVar 
        | IntVar 
        | DoubleVar 
        | StringVar 
        | BlankVar 
        | FunctionVar
        | FunctionDecVar
        | FunctionDefVar
        | VarName
        deriving (Show,Read,Eq,Ord)
        
-- | Vrati typ premennej pre meno existujucej premennej
getVarType :: SymbolTable -> String -> Maybe VarTypes
getVarType [] _ = Nothing
getVarType ((var, _ , FunctionDefVar):vars) v = getVarType vars v
getVarType ((var, _ , FunctionDecVar):vars) v = getVarType vars v
getVarType ((var, _ , varType):vars) v = 
        if v == var
                then Just varType
                else getVarType vars v


isBlankVar :: Expr -> Bool
isBlankVar Blank = True
isBlankVar _ = False

isIntLiteral :: Expr -> Bool
isIntLiteral (LiteralInt _) = True
isIntLiteral _ = False

isDoubleLiteral :: Expr -> Bool
isDoubleLiteral (LiteralDouble _) = True
isDoubleLiteral _ = False

isBoolLiteral :: Expr -> Bool
isBoolLiteral (LiteralBool _) = True
isBoolLiteral _ = False

isStringLiteral :: Expr -> Bool
isStringLiteral (LiteralString _) = True
isStringLiteral _ = False

isBlankLiteral :: Expr -> Bool
isBlankLiteral Blank = True
isBlankLiteral _ = False

isFunction :: Expr -> Bool
isFunction (Function _ _) = True
isFunction _ = False

isVarName :: Expr -> Bool
isVarName (Var _) = True
isVarName _ = False

isExpr :: Expr -> Bool
isExpr (Var _) = True
isExpr (Mult _ _) = True
isExpr (Plus _ _) = True
isExpr (Minus _ _) = True
isExpr (Div _ _) = True
isExpr _ = False

isUnMinusLiteral :: Expr -> Bool
isUnMinusLiteral (UnMinus _) = True
isUnMinusLiteral _ = False

getUnMinusLiteralType :: Expr -> Expr
getUnMinusLiteralType (UnMinus e) = read $ show e

isIntVar :: VarTypes -> Bool
isIntVar IntVar = True
isIntVar _ = False

isDoubleVar :: VarTypes -> Bool
isDoubleVar DoubleVar = True
isDoubleVar _ = False

isBoolVar :: VarTypes -> Bool
isBoolVar BoolVar = True
isBoolVar _ = False

isStringVar :: VarTypes -> Bool
isStringVar StringVar = True
isStringVar _ = False

isFunctionVar :: VarTypes -> Bool
isFunctionVar FunctionVar = True
isFunctionVar _ = False


-- | Vrati typ pre zadanu Expr hodnotu
getValueType :: Expr -> VarTypes
getValueType value
                | isStringLiteral value = StringVar
                | isBoolLiteral value = BoolVar
                | isDoubleLiteral value = DoubleVar
                | isIntLiteral value = IntVar
                | isBlankLiteral value = BlankVar
                | isFunction value = FunctionVar
                | isVarName value = VarName
                | otherwise = error $ show value
