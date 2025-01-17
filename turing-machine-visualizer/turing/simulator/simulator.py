from dataclasses import dataclass
from textwrap import dedent
from typing import Mapping


@dataclass
class TransitionResult:
    state_to_activate: str
    symbol_to_write: str
    direction_to_move: str


Symbol = str
State = str
Transitions = Mapping[tuple[State, Symbol], TransitionResult]
RawTransitions = Mapping[tuple[str, str], tuple[str, str, str]]
Tape = dict[int, Symbol]


class TuringMachine:
    def __init__(self, initial_state: str, tape: Tape, state_transitions: RawTransitions):
        self.tape: Tape = tape
        self.state: State = initial_state
        self.state_transitions: Transitions = {
            (arguments[0], arguments[1]): TransitionResult(result[0], result[1], result[2])
            for arguments, result in state_transitions.items()
        }
        self._tape_position: int = 0

        self.nonfinal_states = set(map(lambda x: x[0], self.state_transitions.keys()))

    def is_in_nonfinal_state(self):
        return self.state in self.nonfinal_states

    def get_tape_contents(self):
        return tuple(map(lambda x: x[1], sorted(self.tape.items())))

    def run_to_final_state(self):
        while self.is_in_nonfinal_state():
            self.step()

    def step(self) -> None:
        current_symbol = self.tape.get(self._tape_position, "")
        transition_result = self.state_transitions.get((self.state, current_symbol), None)

        if transition_result is None:
            raise ValueError(
                dedent("""\
                The Turing Machine reached a non-halting state with no valid transition.
                Your state transition function appears to be incorrect,
                or at least unable to handle the given initial input. """)
            )

        self.tape[self._tape_position] = transition_result.symbol_to_write
        self.state = transition_result.state_to_activate
        self._tape_position += 1 if transition_result.direction_to_move == ">" else -1
        print(
            f"{transition_result.symbol_to_write} {transition_result.direction_to_move} {transition_result.state_to_activate}",
            flush=True,
        )
