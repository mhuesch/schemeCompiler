module Main where

import Control.Monad
import Control.Monad.Error
import System.Environment
import System.IO
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (parserFail)


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    when !runtimeOExists $ putStrLn "No runtime.o. Exiting."
    prog <- liftM generateAssembly $ readFile (args !! 0)
    let filename = "prog.S"
    writeFile filename prog
    rawSystem "as" ["--32", "-o", "prog.o", "prog.S"]
    rawSystem "gcc" ["-m32", "-o", "a.out", "prog.o", "runtime.o"]
    return ()



generateAssembly :: String -> String
generateAssembly input = case parse parseProg "lisp" input of
    Left err -> mainPrefix ++ mainSuffix
    Right val -> assembleProgram val

assembleProgram :: Program -> String
assembleProgram (Program mainBody funs) = fileHeader
                                       ++ mainPrefix
                                       ++ concatMap assembleInstruction mainBody
                                       ++ concatMap assembleFunction funs
                                       ++ mainSuffix
                                       ++ fileFooter

assembleFunction :: Function -> String
assembleFunction (Function label body) = (assembleLabel label) ++ "\n"
                                      ++ concatMap assembleInstruction body

assembleInstruction :: Instruction -> String
assembleInstruction (Assign r s) = "movl " ++ assembleS s ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ReadMem r1 r2 n) = ""
assembleInstruction (Update r n s) = ""
assembleInstruction (Arith r aop t) = ""
assembleInstruction (ShiftSX r1 sop r2) = ""
assembleInstruction (ShiftNum r sop n) = ""
assembleInstruction (SaveCmp r t1 cmp t2) = ""
assembleInstruction (ILab label) = ""
assembleInstruction (Goto label) = ""
assembleInstruction (Cjump t1 cmp t2 l1 l2) = ""
assembleInstruction (Call u) = ""
assembleInstruction (Tail_Call u) = ""
assembleInstruction (Return) = ""
assembleInstruction (Print r t) = "pushl " ++ assembleT t ++ "\ncall print\naddl $4, %esp\n"
assembleInstruction (Allocate r t1 t2) = ""
assembleInstruction (Array_Error r t1 t2) = ""

assembleReg :: Reg -> String
assembleReg (EAX) = "%eax"
assembleReg (ECX) = "%ecx"

assembleNum :: Int -> String
assembleNum n = "$" ++ show n

assembleS :: S -> String
assembleS (Sreg r) = assembleReg r
assembleS (Snum n) = assembleNum n
assembleS (Slab l) = assembleLabel l

assembleT :: T -> String
assembleT (Treg r) = assembleReg r
assembleT (Tnum n) = assembleNum n

assembleLabel :: Label -> String
assembleLabel (Label name) = ":" ++ name

fileHeader :: String
fileHeader = "    .text\n"
          ++ "    .globl go\n"
          ++ "    .type   go, @function\n"
          ++ "go:\n"

fileFooter :: String
fileFooter = "    .size go, .-go\n"
          ++ "    .section    .note.GNU-stack,\"\",@progbits\n"

mainPrefix :: String
mainPrefix = "pushl   %ebp\n"
          ++ "movl    %esp, %ebp\n"
          ++ "pushl   %ebx\n"
          ++ "pushl   %esi\n"
          ++ "pushl   %edi\n"
          ++ "pushl   %ebp\n"
          ++ "movl    %esp, %ebp\n"

mainSuffix :: String
mainSuffix = "popl   %ebp\n"
          ++ "popl   %edi\n"
          ++ "popl   %esi\n"
          ++ "popl   %ebx\n"
          ++ "leave\n"
          ++ "ret\n"


spaces1 :: Parser ()
spaces1 = skipMany1 space


{- L1Error -}
data L1Error = Parser ParseError
             | Default String

instance Show L1Error where
    show = showError

instance Error L1Error where
    noMsg = Default "An error has occurred"
    strMsg = Default

showError :: L1Error -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either L1Error

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
{- End L1Error -}


parseProg :: Parser Program
parseProg = do
    char '('
    main <- parseMain
    whitespaceOrComment
    funs <- parseFunctions
    char ')'
    return $ Program main funs


parseMain :: Parser [Instruction]
parseMain = do
    char '('
    ls <- parseInstructions
    char ')'
    return ls

parseFunctions :: Parser [Function]
parseFunctions = do
    fs <- sepEndBy parseFunction whitespaceOrComment
    return fs

parseFunction :: Parser Function
parseFunction = do
    char '('
    name <- parseLabel
    whitespaceOrComment
    ls <- parseInstructions
    char ')'
    return $ Function name ls

parseInstructions :: Parser [Instruction]
parseInstructions = do
    ls <- sepEndBy parseInstruction whitespaceOrComment
    return ls

whitespaceOrComment :: Parser ()
whitespaceOrComment = do
    spaces
    c <- lookAhead anyChar
    case c of
        '(' -> return ()
        ':' -> return ()
        ')' -> return ()
        ';' -> skipComment
        '\n' -> char '\n' >> whitespaceOrComment
        x -> parserFail $ "Unrecognized: " ++ show x

skipComment :: Parser ()
skipComment = do
    char ';'
    char ';'
    skipMany (noneOf "\n")
    char '\n'
    whitespaceOrComment

parseInstruction :: Parser Instruction
parseInstruction = (liftM ILab parseLabel) <|> do
    char '('
    instr <- parseC
         <|> parseTailCall
         <|> parseReturn
         <|> parseGoto
         <|> parseUpdate
         <|> parseRegOp
    char ')'
    return instr

parseC :: Parser Instruction
parseC = do
    char 'c'
    parseCall <|> parseCjump


parseCall :: Parser Instruction
parseCall = do
    string "all"
    space
    (liftM Call parseU)

parseTailCall :: Parser Instruction
parseTailCall = do
    string "tail-call"
    space
    (liftM Tail_Call parseU)

parseReturn :: Parser Instruction
parseReturn = do
    string "return"
    return Return

parseGoto :: Parser Instruction
parseGoto = do
    string "goto"
    space
    (liftM Goto parseLabel)

parseCjump :: Parser Instruction
parseCjump = do
    string "jump"
    space
    t1 <- parseT
    space
    cmp <- parseCMP
    space
    t2 <- parseT
    space
    l1 <- parseLabel
    space
    l2 <- parseLabel
    return $ Cjump t1 cmp t2 l1 l2

parseUpdate :: Parser Instruction
parseUpdate = do
    string "(mem"
    space
    r <- parseReg
    space
    n <- parseNumber
    string ") <- "
    s <- parseS
    return $ Update r n s

parseRegOp :: Parser Instruction
parseRegOp = do
    reg <- parseReg
    space
    op <- many1 (noneOf " ")
    space
    case op of
        "<-" -> parseArrow reg
        "+=" -> parseArith reg Add
        "-=" -> parseArith reg Sub
        "*=" -> parseArith reg Mult
        "&=" -> parseArith reg And
        "<<=" -> parseShift reg ShiftLeft
        ">>=" -> parseShift reg ShiftRight

parseArrow :: Reg -> Parser Instruction
parseArrow reg = parseArrowParen reg
             <|> try (parseSaveCmp reg)
             <|> parseAssign reg

parseArrowParen :: Reg -> Parser Instruction
parseArrowParen reg = do
    char '('
    tok <- many1 (oneOf (['a'..'z'] ++ ['-']))
    space
    case tok of
        "print" -> do
            t <- parseT
            char ')'
            return $ Print reg t
        "allocate" -> do
            t1 <- parseT
            space
            t2 <- parseT
            char ')'
            return $ Allocate reg t1 t2
        "array-error" -> do
            t1 <- parseT
            space
            t2 <- parseT
            char ')'
            return $ Array_Error reg t1 t2
        "mem" -> do
            r2 <- parseReg
            space
            n <- parseNumber
            char ')'
            return $ ReadMem reg r2 n

parseSaveCmp :: Reg -> Parser Instruction
parseSaveCmp reg = do
    t1 <- parseT
    space
    cmp <- parseCMP
    space
    t2 <- parseT
    return $ SaveCmp reg t1 cmp t2


parseAssign :: Reg -> Parser Instruction
parseAssign reg = do
    s <- parseS
    return $ Assign reg s

parseReg :: Parser Reg
parseReg = do
    tok <- many1 letter
    case tok of
        "eax" -> return EAX
        "ecx" -> return ECX
        "edx" -> return EDX
        "ebx" -> return EBX
        "esi" -> return ESI
        "edi" -> return EDI
        "ebp" -> return EBP
        "esp" -> return ESP

{- parseS -}
parseS :: Parser S
parseS = (liftM Slab parseLabel)
     <|> (liftM Sreg parseReg)
     <|> (liftM Snum parseNumber)

parseLabel :: Parser Label
parseLabel = do
    char ':'
    l <- letter
    ls <- many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))
    return $ Label (l:ls)

parseNumber :: Parser Int
parseNumber = parseNeg <|> parsePos

parseNeg :: Parser Int
parseNeg = do
    char '-'
    n <- many1 digit
    return $ -(read n)

parsePos :: Parser Int
parsePos = do
    n <- many1 digit
    return . read $ n
{- end parseS -}

parseU :: Parser U
parseU = (liftM Ulab parseLabel)
     <|> (liftM Ureg parseReg)

parseT :: Parser T
parseT = (liftM Tnum parseNumber)
     <|> (liftM Treg parseReg)

parseArith :: Reg -> AOP -> Parser Instruction
parseArith reg aop = do
    t <- parseT
    return $ Arith reg aop t

parseShift :: Reg -> SOP -> Parser Instruction
parseShift reg sop = numShift <|> regShift
    where
        numShift = do
            n <- parseNumber
            return $ ShiftNum reg sop n
        regShift = do
            reg2 <- parseReg
            return $ ShiftSX reg sop reg2

parseCMP :: Parser CMP
parseCMP = (char '<' >> ((char '=' >> return LessThan)
                        <|> return LessThanEqual))
       <|> (char '=' >> return Equal)



data Program = Program [Instruction] [Function]
               deriving (Show)

data Function = Function Label [Instruction]
                deriving (Show)


data Instruction = Assign Reg S
                 | ReadMem Reg Reg Int
                 | Update Reg Int S
                 | Arith Reg AOP T
                 | ShiftSX Reg SOP Reg
                 | ShiftNum Reg SOP Int
                 | SaveCmp Reg T CMP T
                 | ILab Label
                 | Goto Label
                 | Cjump T CMP T Label Label
                 | Call U
                 | Tail_Call U
                 | Return
                 | Print Reg T
                 | Allocate Reg T T
                 | Array_Error Reg T T
                 deriving (Show)


data S = Sreg Reg
       | Snum Int
       | Slab Label
       deriving (Show)

data U = Ureg Reg
       | Ulab Label
       deriving (Show)

data T = Treg Reg
       | Tnum Int
       deriving (Show)

{- Registers -}
data Reg = ESI
         | EDI
         | EBP
         | ESP
         | EDX
         | EBX
         | EAX
         | ECX
         deriving (Show)

{- Operators -}
data AOP = Add
         | Sub
         | Mult
         | And
         deriving (Show)

data SOP = ShiftLeft
         | ShiftRight
         deriving (Show)

data CMP = LessThan
         | LessThanEqual
         | Equal
         deriving (Show)

{- Labels -}
data Label = Label String
             deriving (Show)






