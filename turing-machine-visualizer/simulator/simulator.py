from typing import TypeAlias, NewType, Literal, Mapping

FinalState = NewType("FinalState", str)
NonFinalState = NewType("NonFinalState", str)
State = FinalState | NonFinalState

Symbol = NewType("Symbol", str)

Transitions = Mapping[tuple[NonFinalState, Symbol], tuple[State, Symbol, str]]

Tape = Mapping[int, Symbol]


class TuringMachine:
    def __init__(
            self,
            initial_state: State,
            tape: Tape,
            state_transition: Transitions
    ):
        self.tape: Tape = tape
        self.state: State = initial_state

    def step(self) -> None:
        self.tape[0] = "1"
