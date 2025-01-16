from turing.simulator.simulator import TuringMachine


def test_initializes_empty_tape_correctly() -> None:
    initial_state = "Q1"
    turing_machine = TuringMachine(initial_state, {}, {})
    assert turing_machine.tape == {}


def test_writes_symbol_from_transitions() -> None:
    initial_state = "Q1"
    transitions = {(initial_state, ""): (initial_state, "1", ">")}
    turing_machine = TuringMachine(initial_state, {}, transitions)
    turing_machine.step()
    assert turing_machine.tape == {0: "1"}


def test_writes_symbol_and_changes_state() -> None:
    initial_state = "Q1"
    final_state = "Q2"
    transitions = {(initial_state, ""): (final_state, "2", ">")}
    turing_machine = TuringMachine(initial_state, {}, transitions)
    turing_machine.step()
    assert turing_machine.tape == {0: "2"}
    assert turing_machine.state == final_state


def test_writes_hello_world_then_codefreeze() -> None:
    tape_symbols = set("Hello World Codefreeze")
    transitions = {
        ("q0", ""): ("q1", "H", ">"),
        ("q1", ""): ("q2", "e", ">"),
        ("q2", ""): ("q3", "l", ">"),
        ("q3", ""): ("q4", "l", ">"),
        ("q4", ""): ("q5", "o", ">"),
        ("q5", ""): ("q6", " ", ">"),
        ("q6", ""): ("q7", "W", ">"),
        ("q7", ""): ("q8", "o", ">"),
        ("q8", ""): ("q9", "r", ">"),
        ("q9", ""): ("q10", "l", ">"),
        ("q10", ""): ("left_to_space", "d", "<"),
        **{("left_to_space", symbol): ("left_to_space", symbol, "<") for symbol in tape_symbols},
        ("left_to_space", " "): ("q11", " ", ">"),
        ("q11", "W"): ("q12", "C", ">"),
        ("q12", "o"): ("q13", "o", ">"),
        ("q13", "r"): ("q14", "d", ">"),
        ("q14", "l"): ("q15", "e", ">"),
        ("q15", "d"): ("q16", "f", ">"),
        ("q16", ""): ("q17", "r", ">"),
        ("q17", ""): ("q18", "e", ">"),
        ("q18", ""): ("q19", "e", ">"),
        ("q19", ""): ("q20", "z", ">"),
        ("q20", ""): ("q21", "e", ">"),
    }

    turing_machine = TuringMachine("q0", {}, transitions)
    turing_machine.run_to_final_state()
    assert "".join(turing_machine.get_tape_contents()) == "Hello Codefreeze"
