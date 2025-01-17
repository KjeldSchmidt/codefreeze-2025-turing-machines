import time

from turing.simulator.simulator import TuringMachine


def main():
    tape_symbols = set("Hello World Codefreeze")
    transitions = {
        ("q0", ""): ("q1", "H", ">"),
        ("q1", ""): ("q2", "e", ">"),
        ("q2", ""): ("q3", "l", ">"),
        ("q3", ""): ("q4", "l", ">"),
        ("q4", ""): ("q5", "o", ">"),
        ("q5", ""): ("q6", "_", ">"),
        ("q6", ""): ("q7", "W", ">"),
        ("q7", ""): ("q8", "o", ">"),
        ("q8", ""): ("q9", "r", ">"),
        ("q9", ""): ("q10", "l", ">"),
        ("q10", ""): ("left_to_space", "d", "<"),
        **{("left_to_space", symbol): ("left_to_space", symbol, "<") for symbol in tape_symbols},
        ("left_to_space", "_"): ("q11", "_", ">"),
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

    time.sleep(5)
    turing_machine = TuringMachine("q0", {}, transitions)
    while turing_machine.is_in_nonfinal_state():
        turing_machine.step()
        time.sleep(1)
