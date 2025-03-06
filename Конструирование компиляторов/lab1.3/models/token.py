from models.domain_tags import DomainTag
from models.fragment import Fragment
from models.position import Position


class Token:
    def __init__(self, tag: int, starting: Position, following: Position):
        self.tag = tag
        self.coords = Fragment(starting, following)


class IdentToken(Token):
    def __init__(self, code: int, starting: Position, following: Position):
        super().__init__(DomainTag.IDENT.value, starting, following)
        self.code = code


class DirectToken(Token):
    def __init__(self, value: str, starting: Position, following: Position):
        super().__init__(DomainTag.DIRECT.value, starting, following)
        self.value = value


class IntToken(Token):
    def __init__(self, num: int, starting: Position, following: Position):
        super().__init__(DomainTag.INT.value, starting, following)
        self.num = num


class IntDoubleToken(Token):
    def __init__(self, num: int, starting: Position, following: Position):
        super().__init__(DomainTag.INTDOUBLE.value, starting, following)
        self.num = num


class SpecToken(Token):
    def __init__(self, tag: DomainTag, starting: Position, following: Position):
        super().__init__(tag.value, starting, following)
