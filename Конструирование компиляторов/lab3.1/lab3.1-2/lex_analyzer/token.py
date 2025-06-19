from lex_analyzer.fragment import Fragment
from lex_analyzer.position import Position


class Token:
    def __init__(self, name: str, value: str | int, starting: Position, following: Position):
        self.coords = Fragment(starting, following)
        self.value = value
        self.name = name
