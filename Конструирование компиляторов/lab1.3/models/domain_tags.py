import enum


class DomainTag(enum.Enum):
    IDENT = (0,)
    DIRECT = (1,)
    LPAREN = (2,)
    RPAREN = (3,)
    LPARENTR = (4,)
    RPARENTR = (5,)
    INT = (6,)
    INTDOUBLE = (7,)
    EOF = 8
