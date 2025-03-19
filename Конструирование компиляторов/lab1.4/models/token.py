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


class SpaceToken(Token):
    def __init__(self, starting: Position, following: Position):
        super().__init__(DomainTag.SPACE.value, starting, following)


class KeywordToken(Token):
    def __init__(self, value: str, starting: Position, following: Position):
        super().__init__(DomainTag.KEYWORD.value, starting, following)
        self.value = value


class DigToken(Token):
    def __init__(self, num: int, starting: Position, following: Position):
        super().__init__(DomainTag.DIG.value, starting, following)
        self.num = num


class ArrowLRToken(Token):
    def __init__(self, starting: Position, following: Position):
        super().__init__(DomainTag.ARROW_LR.value, starting, following)


class ArrowRLToken(Token):
    def __init__(self, starting: Position, following: Position):
        super().__init__(DomainTag.ARROW_RL.value, starting, following)


class StringToken(Token):
    def __init__(self, value:str, starting: Position, following: Position):
        super().__init__(DomainTag.STRING.value, starting, following)
        self.value = value


class SpecToken(Token):
    def __init__(self, tag: DomainTag, starting: Position, following: Position):
        super().__init__(tag.value, starting, following)
