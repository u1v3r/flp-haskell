----------------------------------------------------------
--   Názov projektu: flp-basic
--   Autori: 
--           * Radovan Dvorský - xdvors08 (veduci)
--           * Ondřej Hanzlík - xhanzl05
--           * Martin Klement - xkleme07
--           * Radim Reš - xresra00         
----------------------------------------------------------
-- | Stara sa o parsovanie dokumentu
module ParserModule where

import InterpreterModule
import VariablesModule

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language


-- | Definicia zakladnej struktury jazyka
aelDef = emptyDef
        { --komentare odstranene pri nacitani suboru, netreba definovat
          identStart     = letter <|> char '_'
        , identLetter    = alphaNum <|> char '_'
        , opStart        = oneOf "=*+"
        , opLetter       = opStart aelDef
        , reservedOpNames= [ "=", "*", "/","+","-","<>","<","<=",">",">="]
        , reservedNames  = [ "as","declare","dim","do","double","else", "end","function",
                             "if","input","integer","loop","print","return","scope","__return_in_scope__",
                             "string","then","while"]
        , caseSensitive  = True--vyriesene pri nacitani vstupu, preto True
        }

lexer = P.makeTokenParser aelDef

whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
float     = P.float lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer--netreba
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
stringLiteral = P.stringLiteral lexer
newLine   = many1 $ symbol "${newline}"
returnInScope = reserved "__return_in_scope__"

       
-- | Definuje vsetky vyrazy a operacie nad nimi
expr = buildExpressionParser operators term where
        operators =        
                [ [ unary "-" UnMinus ]
                , [ binary "*" Mult, binary "/" Div ]
                , [ binary "+" Plus, binary "-" Minus ]
                , [ binary "=" Equals,  binary "<>" NotEquals,binary "<" LeT, binary "<=" LeTE, binary ">" GrT,  binary ">=" GrTE]
                ]
        binary name fun =
                Infix ( do{ reservedOp name; return fun } ) AssocLeft
        unary name fun = 
                Prefix (do{ reservedOp name; return fun })

-- | Obsah vyrazu
term = try (parseDoubleVar) <|> ( parseIntVar) <|> (parseVar) <|> (parseStringVar) <|> (parens expr)

-- | Hlavny parser
aep = do
        whiteSpace
        optional newLine
        ast <- cmd                 --telo programu
        eof
        return ast   
 
-- | Spusti parser     
parseAep input file =
        case parse aep file input of
                Left e -> error "parse scope error"
                Right ast -> ast
                
-- | Parser pre deklaraciu funkcie
fncDeclaration = do
                 reserved "declare"
                 reserved "function"
                 name <- identifier                 
                 params <- parens $ sepBy valueParser parseFncParams                
                 whiteSpace
                 reserved "as"
                 t <- typeParser
                 newLine
                 return $ DeclareFunction (FunctionDec name params t)

-- | Parser pre defniciu funkcie
fncDefinition = do
                 reserved "function"
                 name <- identifier
                 params <- parens $ sepBy valueParser parseFncParams
                 whiteSpace
                 reserved "as"
                 t <- typeParser
                 newLine
                 vars <- many fncVariableDeclaration
                 cmds <- many cmd
                 reserved "return"
                 ret <- expr
                 newLine
                 reserved "end"
                 reserved "function"
                 newLine
                 return $ DefineFunction (FunctionDef name params t cmds vars ret Blank)           

-- | Parsuje identifikator a navratovy typ funkcie
valueParser = do       
                e <- identifier
                reserved "as"
                t <- typeParser
                return (e,Blank,t)--Blank je prazdna hodnota parametru funkcie
              
                
        
-- | Parametre funkcie
parseFncParams = do{
        skipMany space;
        char ',';
        skipMany space
}                           

-- | Parsuje datove typy
typeParser = try (do{reserved "integer"; return IntVar})
                  <|> (do{reserved "string"; return StringVar})
                  <|> (do{reserved "double"; return DoubleVar})
                  <|> (do{reserved "bool"; return BoolVar})

-- | Parsuje bool hodnoty
parseBoolVar =      try (do{     
                    string "true";
                    return $ LiteralBool True
                    })
                    <|> (do{
                    string "false";
                    return $ LiteralBool False
                    })
-- | Parsuje int hodnoty
parseIntVar =       do 
                    v <- integer 
                    return $ LiteralInt v

-- | Parsuje double hodnoty
parseDoubleVar =    do
                    v <- float        
                    return $ LiteralDouble v

-- | Parsuje textove hodnoty                    
parseStringVar =    do
                    v <- stringLiteral     
                    return $ LiteralString v

-- | Parsuje premenne
parseVar       =    do
                    v <- identifier
                    return $ Var v      

-- | Parsuje volanie funkcie
parseFunctionCall = do
                    i <- identifier
                    params <- parens $ sepBy (do{e<-expr;return e}) parseFncParams ;
                    whiteSpace;
                    return $ Function i params

-- | Deklaracia parametru funkcie
fncVariableDeclaration =  do
                reserved "dim"
                i <- identifier
                reserved "as"
                t <- typeParser
                try (do{
                      reservedOp "=";
                      v <- try (parseBoolVar) <|> (expr);
                      newLine;
                      return (i,v,t);                                      
                })
                     <|> (do{
                      newLine;
                      return (i,Blank,t); 
                })
                
-- | Parsuje deklaraciu prip. aj definiciu premennej
variableDeclarationParse = do 
                --premenne maju tvar dim <meno> as <typ>
                reserved "dim"
                i <- identifier
                reserved "as"
                t <- typeParser
                try (do{
                      reservedOp "=";
                      v <- try (parseBoolVar) <|> (expr);
                      newLine;
                      return $ NewVariable i v t;                                      
                })
                     <|> (do{
                      newLine;
                      return $ NewVariable i Blank t
                })

-- | Hlavne telo programu vratane zakladnych prikazov
cmd =   do--kontrola scope<newline>..prikazy...<newline>end<space>scope                
                fncs <- many $ try (do {dec <- fncDeclaration;return dec}) <|> (do{def <- fncDefinition;return def})
                reserved "scope"
                newLine
                cmdSeq <- many cmd        
                reserved "end"                
                reserved "scope"
                newLine 
                return $ Document fncs cmdSeq
        <|> do                     
                i <- identifier;
                --nasleduje parsovanie priradenia alebo parsovanie volania funkcie
                try(do{
                        reservedOp "=";
                        v <- try (parseFunctionCall) <|> (parseBoolVar) <|> (expr);
                        newLine;        
                        return $ Assign i v                        
                        })
                      <|> (do{ 
                        params <- parens $ sepBy (do{e<-expr;return e}) parseFncParams;
                        whiteSpace;
                        newLine;
                        return $ CallFunction i params
                      })
                
        <|> do variableDeclarationParse                
        <|> do
                reserved "print";
                e <- try (parseFunctionCall) <|> (parseBoolVar) <|>  (expr);
                newLine;
                return $ Print e               
        <|> do
                reserved "input"
                v <- identifier
                newLine;
                return $ Input v
        <|> do
                reserved "if"
                b <- expr
                reserved "then"               
                newLine
                cmd1 <- many cmd
                try (do{
                        reserved "else";
                        newLine;
                        cmd2 <- many cmd;
                        reserved "end";
                        reserved "if";
                        newLine;
                        return $ If b (Seq cmd1) (Seq cmd2)                                
                     })
                     <|> (do{
                        reserved "end";
                        reserved "if";
                        newLine;
                        return $ SimpleIf b (Seq cmd1)
                     })               
                
        <|> do
                reserved "do"
                reserved "while"
                b <- expr
                newLine
                c <- many cmd
                reserved "loop"
                newLine
                return $ While b (Seq c)
        <|> do         
                returnInScope
                newLine
                return $ ReturnInScope
        <?> "cmd"  