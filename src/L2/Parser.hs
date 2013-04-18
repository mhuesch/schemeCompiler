module L2.Parser where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Prim (parserFail)

import L2.Grammar


{- Skip 1 or more spaces -}
spaces1 :: Parser ()
spaces1 = skipMany1 space

{- Liveness parser -}
parseLiveness :: Parser [L2Instruction]
parseLiveness = do
    whitespaceOrComment
    m <- parseMain
    return m

{- Spill parsers -}
parseSpill :: Parser L2Spill
parseSpill = do
    whitespaceOrComment
    body <- parseMain
    whitespaceOrComment
    var <- parseVar
    whitespaceOrComment
    offset <- parseNumber
    whitespaceOrComment
    spillPrefix <- parseVar
    return $ L2Spill body var offset spillPrefix


{- Regular L2 parsers -}
readProg :: String -> L2Program
readProg input = case parse parseProg "lisp" input of
    Left err -> L2Program [] []
    Right prog -> prog

readInstrList :: String -> [L2Instruction]
readInstrList input = case parse parseMain "lisp" input of
    Left err -> error $ show err
    Right m -> m

readInstruction :: String -> L2Instruction
readInstruction input = case parse parseInstruction "lisp" input of
    Left err -> error $ show err
    Right instr -> instr

parseProg :: Parser L2Program
parseProg = do
    whitespaceOrComment
    char '('
    whitespaceOrComment
    main <- parseMain
    whitespaceOrComment
    funs <- parseFunctions
    whitespaceOrComment
    char ')'
    return $ L2Program main funs


parseMain :: Parser [L2Instruction]
parseMain = do
    char '('
    whitespaceOrComment
    ls <- parseInstructions
    whitespaceOrComment
    char ')'
    return ls

parseFunctions :: Parser [L2Function]
parseFunctions = do
    fs <- sepBy parseFunction whitespaceOrComment
    return fs

parseFunction :: Parser L2Function
parseFunction = do
    char '('
    name <- parseLabel
    whitespaceOrComment
    ls <- parseInstructions
    whitespaceOrComment
    char ')'
    return $ L2Function name ls

parseInstructions :: Parser [L2Instruction]
parseInstructions = do
    ls <- sepEndBy parseInstruction whitespaceOrComment
    return ls

whitespaceOrComment :: Parser ()
whitespaceOrComment = do
    spaces
    c <- lookAhead anyChar
    case c of
        ';' -> skipComment
        '\n' -> char '\n' >> whitespaceOrComment
        otherwise -> return ()

skipComment :: Parser ()
skipComment = do
    char ';'
    skipMany (noneOf "\n")
    char '\n'
    whitespaceOrComment

parseInstruction :: Parser L2Instruction
parseInstruction = (liftM L2ILab parseLabel) <|> do
    char '('
    whitespaceOrComment
    instr <- (try parseCall)
         <|> (try parseCjump)
         <|> (try parseTailCall)
         <|> (try parseReturn)
         <|> (try parseGoto)
         <|> parseUpdate
         <|> parseRegOp
    whitespaceOrComment
    char ')'
    return instr


parseCall :: Parser L2Instruction
parseCall = do
    string "call"
    whitespaceOrComment
    (liftM L2Call parseU)

parseCjump :: Parser L2Instruction
parseCjump = do
    string "cjump"
    whitespaceOrComment
    t1 <- parseT
    whitespaceOrComment
    cmp <- parseCMP
    whitespaceOrComment
    t2 <- parseT
    whitespaceOrComment
    l1 <- parseLabel
    whitespaceOrComment
    l2 <- parseLabel
    return $ L2Cjump t1 cmp t2 l1 l2

parseTailCall :: Parser L2Instruction
parseTailCall = do
    string "tail-call"
    whitespaceOrComment
    (liftM L2TailCall parseU)

parseReturn :: Parser L2Instruction
parseReturn = do
    string "return"
    return L2Return

parseGoto :: Parser L2Instruction
parseGoto = do
    string "goto"
    whitespaceOrComment
    (liftM L2Goto parseLabel)

parseUpdate :: Parser L2Instruction
parseUpdate = do
    string "(mem"
    whitespaceOrComment
    x <- parseX
    whitespaceOrComment
    n <- parseNumber
    char ')'
    whitespaceOrComment
    string "<-"
    whitespaceOrComment
    s <- parseS
    return $ L2Update x n s

parseRegOp :: Parser L2Instruction
parseRegOp = do
    x <- parseX
    whitespaceOrComment
    op <- many1 (noneOf " ")
    whitespaceOrComment
    case op of
        "<-" -> parseArrow x
        "+=" -> parseArith x L2Add
        "-=" -> parseArith x L2Sub
        "*=" -> parseArith x L2Mult
        "&=" -> parseArith x L2And
        "<<=" -> parseShift x L2ShiftLeft
        ">>=" -> parseShift x L2ShiftRight

parseArrow :: L2X -> Parser L2Instruction
parseArrow x = parseArrowParen x
           <|> try (parseSaveCmp x)
           <|> parseAssign x

parseArrowParen :: L2X -> Parser L2Instruction
parseArrowParen x = do
    char '('
    tok <- many1 (oneOf (['a'..'z'] ++ ['-']))
    whitespaceOrComment
    case tok of
        "print" -> do
            t <- parseT
            spaces
            char ')'
            return $ L2Print t
        "allocate" -> do
            t1 <- parseT
            whitespaceOrComment
            t2 <- parseT
            whitespaceOrComment
            char ')'
            return $ L2Allocate t1 t2
        "array-error" -> do
            t1 <- parseT
            whitespaceOrComment
            t2 <- parseT
            whitespaceOrComment
            char ')'
            return $ L2ArrayError t1 t2
        "mem" -> do
            x2 <- parseX
            whitespaceOrComment
            n <- parseNumber
            whitespaceOrComment
            char ')'
            return $ L2ReadMem x x2 n

parseSaveCmp :: L2X -> Parser L2Instruction
parseSaveCmp x = do
    t1 <- parseT
    whitespaceOrComment
    cmp <- parseCMP
    whitespaceOrComment
    t2 <- parseT
    return $ L2SaveCmp x t1 cmp t2

parseAssign :: L2X -> Parser L2Instruction
parseAssign x = do
    s <- parseS
    return $ L2Assign x s

parseX :: Parser L2X
parseX = do
    v <- oneOf (['_'] ++ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'])
    vs <- many (oneOf (['_'] ++ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
    case (v:vs) of
        "eax" -> retReg L2EAX
        "ecx" -> retReg L2ECX
        "edx" -> retReg L2EDX
        "ebx" -> retReg L2EBX
        "esi" -> retReg L2ESI
        "edi" -> retReg L2EDI
        "ebp" -> retReg L2EBP
        "esp" -> retReg L2ESP
        otherwise -> return . L2Xvar . L2Var $ (v:vs)
    where
        retReg = return . L2Xreg

{- parseS -}
parseS :: Parser L2S
parseS = (liftM L2Slab parseLabel)
     <|> (liftM L2Snum parseNumber)
     <|> (liftM L2SX parseX)

parseLabel :: Parser L2Label
parseLabel = do
    char ':'
    l <- oneOf (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
    ls <- many (oneOf (['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
    return $ L2Label (l:ls)

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

parseU :: Parser L2U
parseU = (liftM L2Ulab parseLabel)
     <|> (liftM L2UX parseX)

parseT :: Parser L2T
parseT = (liftM L2Tnum parseNumber)
     <|> (liftM L2TX parseX)

parseVar :: Parser L2Var
parseVar = do
    v <- oneOf (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
    vs <- many (oneOf (['_'] ++ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
    return $ L2Var (v:vs)

parseArith :: L2X -> L2AOP -> Parser L2Instruction
parseArith x aop = do
    t <- parseT
    return $ L2Arith x aop t

parseShift :: L2X -> L2SOP -> Parser L2Instruction
parseShift x sop = numShift <|> xShift
    where
        numShift = do
            n <- parseNumber
            return $ L2ShiftNum x sop n
        xShift = do
            reg2 <- parseX
            return $ L2ShiftSX x sop reg2

parseCMP :: Parser L2CMP
parseCMP = (char '<' >> ((char '=' >> return L2LessThanEqual)
                        <|> return L2LessThan))
       <|> (char '=' >> return L2Equal)
