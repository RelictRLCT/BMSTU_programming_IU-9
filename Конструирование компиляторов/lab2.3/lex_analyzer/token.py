from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.fragment import Fragment
from lex_analyzer.position import Position


class Token:
    def __init__(self, tag: int, starting: Position, following: Position):
        self.tag = tag
        self.coords = Fragment(starting, following)
        self.value = ''

    def domain(self):
        return DomainTag(self.tag).name


class TermToken(Token):
    def __init__(self, value: str, starting: Position, following: Position):
        super().__init__(DomainTag.TERM.value, starting, following)
        self.value = value


class NTermToken(Token):
    def __init__(self, value: str, starting: Position, following: Position):
        super().__init__(DomainTag.NTERM.value, starting, following)
        self.value = value


class SpecToken(Token):
    def __init__(self, tag: DomainTag, starting: Position, following: Position):
        super().__init__(tag.value, starting, following)
