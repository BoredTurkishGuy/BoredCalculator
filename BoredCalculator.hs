import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad
import Data.Functor.Identity (Identity)
import System.Console.ANSI (clearScreen)

type Variables = Map.Map String Double
type Calculator = StateT Variables IO ()
data Expr = Number Double
          | Var String
          | BinOp String Expr Expr
          | UnOp String Expr
          | Assign String Expr
          | Func String [Expr]
          deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Bored Calculator - A scuffed Haskell project, made by one man"
    putStrLn "Type ':help' for help, ':clear' to clear the screen, or ':quit' to exit."
    evalStateT calculatorLoop Map.empty

calculatorLoop :: Calculator
calculatorLoop = do
    liftIO $ putStr ">> "
    input <- liftIO getLine
    case input of
        ":quit" -> liftIO $ putStrLn "Exiting."
        ":help" -> liftIO displayHelp >> calculatorLoop
        ":clear" -> do
            liftIO clearScreen
            liftIO $ putStrLn "Screen cleared."
            liftIO $ putStrLn "Bored Calculator - Ready for input. Type ':help' for commands."
            calculatorLoop
        ":vars" -> do
            vars <- get
            if Map.null vars
                then liftIO $ putStrLn "No variables defined."
                else liftIO $ printVars vars
            calculatorLoop
        _ -> do
            vars <- get
            case parse (whiteSpace >> expressionParser <* eof) "" input of
                Left err -> liftIO $ putStrLn $ "Error: " ++ show err
                Right expr -> handleExpression vars expr
            calculatorLoop

handleExpression :: Variables -> Expr -> Calculator
handleExpression vars expr = do
    case evaluateExpression vars expr of
        Left errMsg -> liftIO $ putStrLn $ "Error: " ++ errMsg
        Right (value, updatedVars) -> do
            put updatedVars
            liftIO $ putStrLn $ "= " ++ show value

evaluateExpression :: Variables -> Expr -> Either String (Double, Variables)
evaluateExpression vars (Number n) = Right (n, vars)
evaluateExpression vars (Var x) = case Map.lookup x vars of
    Just value -> Right (value, vars)
    Nothing -> Left $ "Undefined variable: " ++ x
evaluateExpression vars (BinOp op a b) = do
    (aVal, _) <- evaluateExpression vars a
    (bVal, _) <- evaluateExpression vars b
    case op of
        "+" -> Right (aVal + bVal, vars)
        "-" -> Right (aVal - bVal, vars)
        "*" -> Right (aVal * bVal, vars)
        "/" -> if bVal /= 0 then Right (aVal / bVal, vars) else Left "Division by zero"
        "^" -> Right (aVal ** bVal, vars)
        "%" -> if bVal /= 0 then Right (aVal - bVal * fromIntegral (floor (aVal / bVal)), vars) else Left "Division by zero in modulo"
        "//" -> Right (fromIntegral (floor (aVal / bVal)), vars)
        "max" -> Right (max aVal bVal, vars)
        "min" -> Right (min aVal bVal, vars)
        _   -> Left $ "Unknown operator: " ++ op
evaluateExpression vars (UnOp op a) = do
    (aVal, _) <- evaluateExpression vars a
    case op of
        "neg" -> Right (-aVal, vars)
        "abs" -> Right (abs aVal, vars)
        _     -> Left $ "Unknown unary operator: " ++ op
evaluateExpression vars (Assign name expr) = do
    (value, _) <- evaluateExpression vars expr
    let updatedVars = Map.insert name value vars
    Right (value, updatedVars)
evaluateExpression vars (Func name args) = do
    argVals <- mapM (\arg -> fst <$> evaluateExpression vars arg) args
    case (name, argVals) of
        ("sin", [x])  -> Right (sin x, vars)
        ("cos", [x])  -> Right (cos x, vars)
        ("tan", [x])  -> Right (tan x, vars)
        ("log", [x])  -> if x > 0 then Right (log x, vars) else Left "Logarithm of non-positive number"
        ("exp", [x])  -> Right (exp x, vars)
        ("sqrt", [x]) -> if x >= 0 then Right (sqrt x, vars) else Left "Square root of negative number"
        ("factorial", [x]) -> if x >= 0 && fromIntegral (round x) == x
                              then Right (fromIntegral $ product [1..round x], vars)
                              else Left "Factorial requires a non-negative integer"
        ("gcd", [x, y]) -> Right (fromIntegral $ gcd (round x) (round y), vars)
        ("lcm", [x, y]) -> Right (fromIntegral $ lcm (round x) (round y), vars)
        ("fibonacci", [x]) -> if x >= 0 && fromIntegral (round x) == x
                              then Right (fromIntegral $ fib (round x), vars)
                              else Left "Fibonacci requires a non-negative integer"
        ("prime", [x]) -> Right (if isPrime (round x) then 1 else 0, vars)
        ("nthprime", [x]) -> if x > 0 && fromIntegral (round x) == x
                             then Right (fromIntegral $ nthPrime (round x), vars)
                             else Left "nthPrime requires a positive integer"
        _             -> Left $ "Unknown function or invalid arguments: " ++ name

fib :: Int -> Int
fib n = fst $ foldl (\(a, b) _ -> (b, a + b)) (0, 1) [1..n]

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..floor . sqrt $ fromIntegral n]

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)
  where primes = filter isPrime [2..]

displayHelp :: IO ()
displayHelp = do
    putStrLn "Bored Calculator - Help"
    putStrLn "Available Commands:"
    putStrLn ":help - Show this help message"
    putStrLn ":clear - Clear the screen"
    putStrLn ":vars - List all defined variables"
    putStrLn ":quit - Exit the calculator"
    putStrLn ""
    putStrLn "Features:"
    putStrLn "1. Basic arithmetic operations: +, -, *, /, ^, %, //"
    putStrLn "2. Variable assignment: x = 5"
    putStrLn "3. Mathematical functions:"
    putStrLn "   sin(x), cos(x), tan(x), log(x), exp(x), sqrt(x), abs(x)"
    putStrLn "   factorial(x), gcd(x, y), lcm(x, y), fibonacci(x)"
    putStrLn "   prime(x), nthprime(x)"
    putStrLn "   min(x, y), max(x, y)"
    putStrLn ""

printVars :: Variables -> IO ()
printVars vars = forM_ (Map.toList vars) $ \(k, v) -> putStrLn $ k ++ " = " ++ show v

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = Language.emptyDef {
        Token.commentLine = "#",
        Token.reservedOpNames = ["+", "-", "*", "/", "^", "%", "//", "=", "(", ")", ","],
        Token.identStart = letter,
        Token.identLetter = alphaNum <|> oneOf "_'"
    }

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

expressionParser :: Parser Expr
expressionParser = try assignmentParser <|> operatorParser

assignmentParser :: Parser Expr
assignmentParser = do
    name <- identifier
    reservedOp "="
    expr <- operatorParser
    return $ Assign name expr

operatorParser :: Parser Expr
operatorParser = buildExpressionParser operatorTable termParser

operatorTable :: OperatorTable String () Identity Expr
operatorTable = [
    [Prefix (reservedOp "-" >> return (UnOp "neg")), Prefix (reservedOp "+" >> return (UnOp "pos"))],
    [Infix (reservedOp "^" >> return (BinOp "^")) AssocRight],
    [Infix (reservedOp "*" >> return (BinOp "*")) AssocLeft, Infix (reservedOp "/" >> return (BinOp "/")) AssocLeft, Infix (reservedOp "%" >> return (BinOp "%")) AssocLeft],
    [Infix (reservedOp "+" >> return (BinOp "+")) AssocLeft, Infix (reservedOp "-" >> return (BinOp "-")) AssocLeft],
    [Infix (reservedOp "//" >> return (BinOp "//")) AssocLeft]
    ]

termParser :: Parser Expr
termParser = parens expressionParser <|> functionParser <|> variableParser <|> numberParser

numberParser :: Parser Expr
numberParser = do
    value <- try float <|> fmap fromIntegral integer
    return $ Number value

variableParser :: Parser Expr
variableParser = Var <$> identifier

functionParser :: Parser Expr
functionParser = do
    funcName <- identifier
    args <- parens $ commaSep expressionParser
    return $ Func funcName args
