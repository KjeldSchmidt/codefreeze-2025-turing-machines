import sys
import textwrap
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Literal, TypeAlias, cast, get_args

from PySide6.QtCore import QSocketNotifier
from PySide6.QtGui import QBrush, QColor
from PySide6.QtWidgets import (
    QApplication,
    QGraphicsRectItem,
    QGraphicsScene,
    QGraphicsTextItem,
    QGraphicsView,
    QMainWindow,
    QVBoxLayout,
    QWidget,
)

window_height = 600
window_width = 800

ValidMove: TypeAlias = Literal[">", "-", "<"]
move_to_tape_head_position_change: Mapping[ValidMove, int] = {">": 1, "-": 0, "<": -1}


@dataclass
class TuringCell:
    rect: QGraphicsRectItem
    text: QGraphicsTextItem

    def set_plain_text(self, new_text: str) -> None:
        self.text.setPlainText(new_text)

        bounding_box = self.rect.rect()

        # Set initial font size and adjust text width to fit within the bounding_box's width
        font = self.text.font()
        for size in range(24, 2, -2):  # Decrease font size until it fits
            font.setPointSize(size)
            self.text.setFont(font)
            if self.text.boundingRect().width() <= bounding_box.width():
                break

        # Center the text inside the rectangle
        text_rect = self.text.boundingRect()
        x_offset = bounding_box.x() + (bounding_box.width() - text_rect.width()) / 2
        y_offset = bounding_box.y() + (bounding_box.height() - text_rect.height()) / 2
        self.text.setPos(x_offset, y_offset)


class TuringMachineVisualizer(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.setWindowTitle("Turing Machine Visualizer")
        self.resize(window_width, window_height)

        # Main widget and layout
        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        layout = QVBoxLayout(main_widget)

        # Graphics view and scene
        self.scene = QGraphicsScene()
        self.view = QGraphicsView(self.scene)
        layout.addWidget(self.view)

        # Tape representation
        self.tape_cells: dict[int, TuringCell] = {}
        self.cell_width = 50
        self.tape_length = 15

        # Background color
        self.view.setBackgroundBrush(QBrush(QColor("white")))

        # State representation
        self.state_text = QGraphicsTextItem("Ready")
        self.state_text.setDefaultTextColor(QColor("black"))
        state_font = self.state_text.font()
        state_font.setPointSize(24)
        self.state_text.setFont(state_font)
        self.state_text.setX(self.cell_width / 2 - self.state_text.boundingRect().width() / 2)
        self.scene.addItem(self.state_text)

        # Step counter
        self.step_count = 0
        self.step_count_text = QGraphicsTextItem(str(self.step_count))
        self.step_count_text.setDefaultTextColor(QColor("black"))
        state_font = self.step_count_text.font()
        state_font.setPointSize(24)
        self.step_count_text.setFont(state_font)
        self.step_count_text.setX(self.cell_width / 2 - self.step_count_text.boundingRect().width() / 2)
        self.step_count_text.setY(self.cell_width * 4)
        self.scene.addItem(self.step_count_text)

        # Simulated Turing machine state
        self.head_position = 0

        # Initial cell
        self.create_cell(0)

        # Add margins for centering
        self.left_margin = QGraphicsRectItem(-100000, 0, 1, 1)
        self.scene.addItem(self.left_margin)
        self.right_margin = QGraphicsRectItem(100000, 0, 1, 1)
        self.scene.addItem(self.right_margin)

        self.notifier = QSocketNotifier(sys.stdin.fileno(), QSocketNotifier.Type.Read)
        self.notifier.activated.connect(self.handle_stdin)

    def handle_stdin(self) -> None:
        line = sys.stdin.readline().strip()
        orange_stdout = "\033[0;33m"
        uncolored_stdout = "\033[0m"

        if line.startswith("[info]"):
            return

        # Stop handling input if a pipe is closed.
        # This cannot be the best way to detect this, right?
        # Sometimes a pipe just pipes in a newline???
        # Well, not in our case
        if line == "" and not sys.stdin.isatty():
            self.notifier.setEnabled(False)
            print(
                textwrap.dedent("""\
                End of input reached - stop receiving new input

                If this was not your intention, please note that this program cannot
                currently handle newlines if a process has been connected via pipe.""")
            )
            return

        if line.startswith("state+tape: ") and self.step_count == 0:
            state_and_tape_symbols = line.split(":")[1].strip().split()
            state, tape_symbols = state_and_tape_symbols[0], state_and_tape_symbols[1:]

            self.state_text.setPlainText(state)
            self.state_text.setX(self.cell_width / 2 - self.state_text.boundingRect().width() / 2)

            for index, symbol in enumerate(tape_symbols):
                self.create_cell(index, symbol)

            return

        if line.startswith("state: ") and self.step_count == 0:
            state = line.removeprefix("state: ").strip()
            self.state_text.setPlainText(state)
            self.state_text.setX(self.cell_width / 2 - self.state_text.boundingRect().width() / 2)
            return

        try:
            new_symbol, move, new_state = line.strip().split()
        except ValueError:
            print(
                textwrap.dedent(f"""
                {orange_stdout}Input needs to be given as:

                new_cell_value direction new_state_name

                where direction is either `<`, `>` or `-`, which makes the tapehead
                move left, right, or not at all respectively.

                new_cell_value and new_state_name can be arbitrary strings{uncolored_stdout}""")
            )
            return

        if move not in get_args(ValidMove):
            print(
                f"{orange_stdout}Move was not given as `<`, `>` or `-`. Any other values are invalid.{uncolored_stdout}"
            )
            return
        move = cast(ValidMove, move)

        self.update_tape(new_symbol, move, new_state)

    def create_cell(self, cell_index: int, symbol: str = "") -> None:
        cell_x = cell_index * self.cell_width
        cell_y = 100
        rect = QGraphicsRectItem(cell_x, cell_y, self.cell_width, self.cell_width)
        rect.setBrush(QBrush(QColor("white")))
        rect.setPen(QColor("black"))
        self.scene.addItem(rect)

        text = QGraphicsTextItem("")
        text.setDefaultTextColor(QColor("black"))
        text.setPos(cell_x + 10, cell_y + 5)
        self.scene.addItem(text)

        turing_cell = TuringCell(rect, text)
        turing_cell.set_plain_text(symbol)
        self.tape_cells[cell_index] = turing_cell

    def update_tape(self, new_symbol: str, move: ValidMove, new_state: str) -> None:
        previous_cell = self.tape_cells[self.head_position]

        # Change current symbol, unhighlight tapehead
        previous_cell.set_plain_text(new_symbol)
        previous_cell.rect.setBrush(QBrush(QColor("white")))

        # Move tapehead
        self.head_position += move_to_tape_head_position_change[move]

        # Create new cell if cellhead moves out of previous bounds
        if self.head_position not in self.tape_cells:
            self.create_cell(self.head_position)

        new_cell = self.tape_cells[self.head_position]

        self.view.centerOn(new_cell.rect)

        # Highlight the current head position
        new_cell.rect.setBrush(QBrush(QColor("yellow")))

        # Update state display
        self.state_text.setPlainText(new_state)
        self.state_text.setX(
            (self.head_position + 0.5) * self.cell_width - 0.5 * self.state_text.boundingRect().width()
        )

        self.step_count += 1
        self.step_count_text.setPlainText(str(self.step_count))
        self.step_count_text.setX(
            (self.head_position + 0.5) * self.cell_width - 0.5 * self.step_count_text.boundingRect().width()
        )


def main() -> None:
    app = QApplication(sys.argv)

    visualizer = TuringMachineVisualizer()
    visualizer.show()

    app.exec()


if __name__ == "__main__":
    main()
