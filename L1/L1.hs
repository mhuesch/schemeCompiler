{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import System.Environment
import System.IO
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Prim (parserFail)


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    when (not runtimeOExists) $ putStrLn "No runtime.o. Exiting."
    contents <- readFile (args !! 0)
    let filename = "prog.S"
        (prog, _) = runState (generateAssembly contents) 0
    writeFile filename prog
    rawSystem "as" ["--32", "-o", "prog.o", "prog.S"]
    rawSystem "gcc" ["-m32", "-o", "a.out", "prog.o", "runtime.o"]
    return ()



generateAssembly :: MonadState Int m => String -> m String
generateAssembly input = case parse parseProg "lisp" input of
    Left err -> return $ fileHeader ++ mainPrefix ++ mainSuffix ++ fileFooter
    Right val -> assembleProgram val

assembleProgram :: MonadState Int m => Program -> m String
assembleProgram (Program mainBody funs) = do
    mainAssem <- liftM concat $ mapM assembleInstruction mainBody
    funAssem <- liftM concat $ mapM assembleFunction funs
    return $ fileHeader
          ++ mainPrefix
          ++ mainAssem
          ++ mainSuffix
          ++ funAssem
          ++ fileFooter

assembleFunction :: MonadState Int m => Function -> m String
assembleFunction (Function label body) = do
    bodyAssem <- liftM concat $ mapM assembleInstruction body
    return $ funLabel ++ bodyAssem
    where
        funLabel = (colonLabel label) ++ "\n"

assembleInstruction :: MonadState Int m => Instruction -> m String
assembleInstruction (Assign r s) = return $ "movl " ++ assembleS s ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ReadMem r1 r2 n) = return $ "movl " ++ show n ++ "(" ++ assembleReg r2 ++ "), " ++ assembleReg r1 ++ "\n"
assembleInstruction (Update r n s) = return $ "movl " ++ assembleS s ++ ", " ++ show n ++ "(" ++ assembleReg r ++ ")\n"
assembleInstruction (Call u) = do
    count <- get
    let newlabel = "_fun_ret_" ++ show count
    put $ count + 1
    return $ "pushl $" ++ newlabel ++ "\npushl %ebp\nmovl %esp, %ebp\njmp " ++ assembleU u ++ "\n" ++ newlabel ++ ":\n"


-- Arith op
assembleInstruction (Arith r Add t) = return $ "addl " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r Sub t) = return $ "subl " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r Mult t) = return $ "imull " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r And t) = return $ "and " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"

-- Shift op
assembleInstruction (ShiftSX r1 ShiftLeft r2) = return $ "sall %cl, " ++ assembleReg r1 ++ "\n"
assembleInstruction (ShiftSX r1 ShiftRight r2) = return $ "sarl %cl, " ++ assembleReg r1 ++ "\n"
assembleInstruction (ShiftNum r ShiftLeft n) = return $ "sall " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ShiftNum r ShiftRight n) = return $ "sarl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n"

-- SaveCmp
-- num - num
assembleInstruction (SaveCmp r (Tnum n1) LessThan (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 < n2) ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n1) LessThanEqual (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 <= n2) ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n1) Equal (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 == n2) ++ ", " ++ assembleReg r ++ "\n"
-- reg - reg
assembleInstruction (SaveCmp r (Treg r2) LessThan (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsetl " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) LessThanEqual (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsetle " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) Equal (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
-- num - reg
assembleInstruction (SaveCmp r (Tnum n) LessThan (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetg " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n) LessThanEqual (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetge " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n) Equal (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
-- reg - num
assembleInstruction (SaveCmp r (Treg r2) LessThan (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetl " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) LessThanEqual (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetle " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) Equal (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"

-- Cjump
-- reg - reg
assembleInstruction (Cjump (Treg r1) LessThan (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\njl " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r1) LessThanEqual (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\njle " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r1) Equal (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- reg - constant
assembleInstruction (Cjump (Treg r) LessThan (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njl " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r) LessThanEqual (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njle " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r) Equal (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- constant - reg
assembleInstruction (Cjump (Tnum n) LessThan (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njg " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Tnum n) LessThanEqual (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njge " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Tnum n) Equal (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- constant - constant
assembleInstruction (Cjump (Tnum n1) LessThan (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 < n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"
assembleInstruction (Cjump (Tnum n1) LessThanEqual (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 <= n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"
assembleInstruction (Cjump (Tnum n1) Equal (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 == n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"

assembleInstruction (ILab label) = return $ colonLabel label ++ "\n"
assembleInstruction (Goto label) = return $ "jmp " ++ underscoreLabel label ++ "\n"
assembleInstruction (Tail_Call u) = return $ "movl %ebp, %esp\n    jmp " ++ assembleU u ++ "\n"
assembleInstruction (Return) = return $ "movl %ebp, %esp\n    pop %ebp\n    ret\n"
assembleInstruction (Print r t) = return $ "pushl " ++ assembleT t ++ "\ncall print\naddl $4, %esp\n"
assembleInstruction (Allocate r t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall allocate\n" ++ "addl $8,%esp\n"
assembleInstruction (Array_Error r t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall print_error\n" ++ "addl $8,%esp\n"

reg8Bit :: Reg -> String
reg8Bit (EAX) = "%al"
reg8Bit (ECX) = "%cl"
reg8Bit (EDX) = "%dl"
reg8Bit (EBX) = "%bl"

assembleCompare :: Bool -> String
assembleCompare test = if test
                          then assembleConstant 1
                          else assembleConstant 0

assembleReg :: Reg -> String
assembleReg (EAX) = "%eax"
assembleReg (ECX) = "%ecx"
assembleReg (ESI) = "%esi"
assembleReg (EDI) = "%edi"
assembleReg (EBP) = "%ebp"
assembleReg (ESP) = "%esp"
assembleReg (EDX) = "%edx"
assembleReg (EBX) = "%ebx"

assembleConstant :: Int -> String
assembleConstant n = "$" ++ show n

assembleS :: S -> String
assembleS (Sreg r) = assembleReg r
assembleS (Snum n) = assembleConstant n
assembleS (Slab l) = "$" ++ underscoreLabel l

assembleT :: T -> String
assembleT (Treg r) = assembleReg r
assembleT (Tnum n) = assembleConstant n

assembleU :: U -> String
assembleU (Ureg r) = "*" ++ assembleReg r
assembleU (Ulab l) = underscoreLabel l

colonLabel :: Label -> String
colonLabel (Label name) = "_" ++ name ++ ":"

underscoreLabel :: Label -> String
underscoreLabel (Label name) = "_" ++ name

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
    whitespaceOrComment
    char '('
    whitespaceOrComment
    main <- parseMain
    whitespaceOrComment
    funs <- parseFunctions
    whitespaceOrComment
    char ')'
    return $ Program main funs


parseMain :: Parser [Instruction]
parseMain = do
    char '('
    whitespaceOrComment
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
    spaces1
    (liftM Call parseU)

parseTailCall :: Parser Instruction
parseTailCall = do
    string "tail-call"
    spaces1
    (liftM Tail_Call parseU)

parseReturn :: Parser Instruction
parseReturn = do
    string "return"
    return Return

parseGoto :: Parser Instruction
parseGoto = do
    string "goto"
    spaces1
    (liftM Goto parseLabel)

parseCjump :: Parser Instruction
parseCjump = do
    string "jump"
    spaces1
    t1 <- parseT
    spaces1
    cmp <- parseCMP
    spaces1
    t2 <- parseT
    spaces1
    l1 <- parseLabel
    spaces1
    l2 <- parseLabel
    return $ Cjump t1 cmp t2 l1 l2

parseUpdate :: Parser Instruction
parseUpdate = do
    string "(mem"
    spaces1
    r <- parseReg
    spaces1
    n <- parseNumber
    char ')'
    spaces1
    string "<-"
    spaces1
    s <- parseS
    return $ Update r n s

parseRegOp :: Parser Instruction
parseRegOp = do
    reg <- parseReg
    spaces1
    op <- many1 (noneOf " ")
    spaces1
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
    spaces1
    case tok of
        "print" -> do
            t <- parseT
            char ')'
            return $ Print reg t
        "allocate" -> do
            t1 <- parseT
            spaces1
            t2 <- parseT
            char ')'
            return $ Allocate reg t1 t2
        "array-error" -> do
            t1 <- parseT
            spaces1
            t2 <- parseT
            char ')'
            return $ Array_Error reg t1 t2
        "mem" -> do
            r2 <- parseReg
            spaces1
            n <- parseNumber
            char ')'
            return $ ReadMem reg r2 n

parseSaveCmp :: Reg -> Parser Instruction
parseSaveCmp reg = do
    t1 <- parseT
    spaces1
    cmp <- parseCMP
    spaces1
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
parseCMP = (char '<' >> ((char '=' >> return LessThanEqual)
                        <|> return LessThan))
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






