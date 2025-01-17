import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad (mplus)
import System.IO (hFlush, stdout)

data Symbol = Symbol String | Any deriving (Show, Eq, Ord)
newtype State = State String deriving (Eq, Ord)
instance Show State where
    show (State s) = s
data Direction = MoveLeft | MoveRight | Stay deriving (Show)

type Transition = ((State, Symbol), (State, Symbol, Direction))
type Transitions = Map (State, Symbol) (State, Symbol, Direction)
type Tape = Map Int Symbol

runTuringMachine :: State -> Transitions -> Tape -> IO ()
runTuringMachine initialState transitions initialTape = do
    putStrLn "[info] Press Enter to execute one step, or enter a number to execute that many steps."
    hFlush stdout
    loop initialTape initialState 0 0
    where
        loop :: Tape -> State -> Int -> Int -> IO ()
        loop tape state pos steps = do
            input <- getLine
            case executeStep tape state pos transitions of
                Nothing -> 
                    putStrLn $ "Machine halted - no transition found after " ++ show steps ++ " steps"
                Just (newTape, nextState, newPos, output) -> do
                    putStrLn output
                    hFlush stdout
                    case reads input of
                        [(n, "")] -> executeSteps (n-1) newTape nextState newPos (steps + 1)
                        _ -> loop newTape nextState newPos (steps + 1)

        executeSteps :: Int -> Tape -> State -> Int -> Int -> IO ()
        executeSteps 0 tape state pos steps = loop tape state pos steps
        executeSteps n tape state pos steps = 
            case executeStep tape state pos transitions of
                Nothing -> 
                    putStrLn $ "Machine halted - no transition found after " ++ show steps ++ " steps"
                Just (newTape, nextState, newPos, output) -> do
                    putStrLn output
                    hFlush stdout
                    executeSteps (n-1) newTape nextState newPos (steps + 1)

executeStep :: Tape -> State -> Int -> Transitions -> Maybe (Tape, State, Int, String)
executeStep tape state pos transitions = do
    let currentSymbol = fromMaybe (Symbol "") $ Map.lookup pos tape
    (nextState, nextSymbol, direction) <- findTransition (state, currentSymbol) transitions
    
    let symbolToWrite = case nextSymbol of
            Any -> currentSymbol
            _ -> nextSymbol
    let newTape = Map.insert pos symbolToWrite tape
    
    let dirSymbol = directionToSymbol direction
    let sym = case nextSymbol of
            Symbol "" -> "_"
            Symbol s -> s
            Any -> case currentSymbol of
                Symbol "" -> "_"
                Symbol s -> s
                Any -> "*"
    let State st = nextState
    let output = unwords [sym, dirSymbol, st]
    
    let newPos = case direction of
            MoveLeft -> pos - 1
            MoveRight -> pos + 1
            Stay -> pos
    
    return (newTape, nextState, newPos, output)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            fileContent <- readFile fileName
            let (initialState, transitions) = parseMapFromFile fileContent
            putStrLn $ "state: " ++ show initialState
            hFlush stdout
            runTuringMachine initialState transitions Map.empty
        [fileName, initialTapeStr] -> do
            fileContent <- readFile fileName
            let (initialState, transitions) = parseMapFromFile fileContent
                initialTape = parseInitialTape initialTapeStr
            putStrLn $ "state+tape: " ++ show initialState ++ " " ++ initialTapeStr
            hFlush stdout
            runTuringMachine initialState transitions initialTape
        _ -> putStrLn "Usage: program <filename> [initial tape]"


parseMapFromFile :: String -> (State, Transitions)
parseMapFromFile content =
    case lines content of
        [] -> error "Empty file"
        (initialState:restLines) -> 
            let nonEmptyLines = filter (not . null) restLines
                parsedTransitions = map (parseTransition . words) nonEmptyLines
            in (State initialState, Map.fromList parsedTransitions)


parseTransition :: [String] -> Transition
parseTransition [s1, sym1, s2, sym2, dir] =
    ((State s1, parseSymbol sym1), 
     (State s2, parseSymbol sym2, 
      parseDirection dir))
parseTransition _ = error "Invalid transition format"


parseDirection :: String -> Direction
parseDirection dir = case dir of
    "<" -> MoveLeft
    ">" -> MoveRight
    "-" -> Stay
    _ -> error "Invalid direction format"


parseSymbol :: String -> Symbol
parseSymbol "*" = Any
parseSymbol "_" = Symbol ""
parseSymbol s = Symbol s


findTransition :: (State, Symbol) -> Transitions -> Maybe (State, Symbol, Direction)
findTransition (state, symbol) transitions = 
    Map.lookup (state, symbol) transitions 
    `mplus` Map.lookup (state, Any) transitions


directionToSymbol :: Direction -> String
directionToSymbol MoveLeft = "<"
directionToSymbol MoveRight = ">"
directionToSymbol Stay = "-"


parseInitialTape :: String -> Tape
parseInitialTape str = 
    Map.fromList $ zip [0..] $ map (Symbol . symbolStr) $ words str
    where symbolStr "_" = ""
          symbolStr s = s
