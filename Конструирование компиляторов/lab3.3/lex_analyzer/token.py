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


class IdentToken(Token):
    def __init__(self, code: int, starting: Position, following: Position):
        super().__init__(DomainTag.IDENT.value, starting, following)
        self.code = code


class SpecToken(Token):
    def __init__(self, tag: DomainTag, starting: Position, following: Position):
        super().__init__(tag.value, starting, following)
