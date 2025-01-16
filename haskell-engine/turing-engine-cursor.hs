import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Control.Monad (mplus)

data Symbol = Symbol String | Any deriving (Show, Eq, Ord)
newtype State = State String deriving (Show, Eq, Ord)
data Direction = MoveLeft | MoveRight | Stay deriving (Show)

type Transition = ((State, Symbol), (State, Symbol, Direction))
type Transitions = Map (State, Symbol) (State, Symbol, Direction)
type Tape = Map Int Symbol

runTuringMachine :: State -> Transitions -> IO ()
runTuringMachine initialState transitions = do
    let initialTape = Map.singleton 0 (Symbol "")
    loop initialTape initialState 0
    where
        loop :: Tape -> State -> Int -> IO ()
        loop tape currentState headPos = do
            let currentSymbol = fromMaybe (Symbol "") $ Map.lookup headPos tape
            case findTransition (currentState, currentSymbol) transitions of
                Nothing -> do
                    putStrLn "Machine halted - no transition found"
                Just (nextState, nextSymbol, direction) -> do
                    
                    -- Keep existing symbol if nextSymbol is Any
                    let symbolToWrite = case nextSymbol of
                            Any -> currentSymbol
                            _ -> nextSymbol
                    let newTape = Map.insert headPos symbolToWrite tape
                    
                    let dirSymbol = directionToSymbol direction
                    -- Print the transition output in required format
                    let sym = case nextSymbol of
                            Symbol "" -> "_"
                            Symbol s -> s
                            Any -> "*"
                    let State st = nextState
                    putStrLn $ unwords [sym, dirSymbol, st]
                    
                    let newHeadPos = case direction of
                            MoveLeft -> headPos - 1
                            MoveRight -> headPos + 1
                            Stay -> headPos
                    loop newTape nextState newHeadPos

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            fileContent <- readFile fileName
            let (initialState, transitions) = parseMapFromFile fileContent
            runTuringMachine initialState transitions
        _ -> putStrLn "Usage: program <filename>"


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
