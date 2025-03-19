import enum


class DomainTag(enum.Enum):
    IDENT = 0
    SPACE = 1
    KEYWORD = 2
    DIG = 3
    ARROW_LR = 4
    ARROW_RL = 5
    STRING = 6
    EOF = 7
