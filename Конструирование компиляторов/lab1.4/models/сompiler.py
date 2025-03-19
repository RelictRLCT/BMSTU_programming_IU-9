from models.message import Message
from models.position import Position
from models.scanner import Scanner


class Compiler:
    def __init__(self):
        self.messages: dict[Position, Message] = {}
        self.namecodes: dict[str, int] = {}
        self.names: list[str] = []

    def add_name(self, name: str) -> int:
        if self.namecodes.get(name) is not None:
            return self.namecodes[name]
        code = len(self.names)
        self.names.append(name)
        self.namecodes[name] = code
        return code

    def get_name(self, code: int) -> str:
        return self.names[code]

    def add_message(self, iserror: bool, c: Position, text: str) -> None:
        self.messages[c] = Message(iserror, text)

    def output_messages(self):
        for p, m in self.messages.items():
            if m.iserror:
                print(f"Error {p.get_coords()}: {m.text}")
            else:
                print(f"Warning {p.get_coords()}: {m.text}")

    def get_scanner(self, program: str) -> Scanner:
        return Scanner(program, self)
