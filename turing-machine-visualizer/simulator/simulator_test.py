from simulator.simulator import TuringMachine, State, NonFinalState, Transitions, Symbol


def test_initializes_empty_tape_correctly() -> None:
    initial_state: NonFinalState = NonFinalState("Q1")
    turing_machine = TuringMachine(initial_state, {}, {})
    assert turing_machine.tape == {}


def test_writes_symbol_from_transition() -> None:
    initial_state: NonFinalState = NonFinalState("Q1")
    transition: Transitions = {
        (initial_state, Symbol("")): (initial_state, Symbol("1"), ">")
    }
    turing_machine = TuringMachine(initial_state, {}, transition)
    turing_machine.step()
    assert turing_machine.tape == {0: "1"}
