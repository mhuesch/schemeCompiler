module L1.Parser
( readProg
) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Prim (parserFail)

import L1.Grammar


{- Skip 1 or more spaces -}
spaces1 :: Parser ()
spaces1 = skipMany1 space

readProg :: String -> Program
readProg input = case parse parseProg "lisp" input of
    Left err -> Program [] []
    Right prog -> prog

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
    spaces
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
            spaces
            char ')'
            return $ Print reg t
        "allocate" -> do
            t1 <- parseT
            spaces1
            t2 <- parseT
            spaces
            char ')'
            return $ Allocate reg t1 t2
        "array-error" -> do
            t1 <- parseT
            spaces1
            t2 <- parseT
            spaces
            char ')'
            return $ Array_Error reg t1 t2
        "mem" -> do
            r2 <- parseReg
            spaces1
            n <- parseNumber
            spaces
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
    l <- oneOf (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
    ls <- many (oneOf (['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
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
