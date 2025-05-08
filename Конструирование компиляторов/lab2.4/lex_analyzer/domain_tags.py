import enum


class DomainTag(enum.Enum):
    IDENT = 0
    KW_CLASS = 1
    KW_TOKENS = 2
    KW_TYPES = 3
    KW_METHODS = 4
    KW_GRAMMAR = 5
    KW_AXIOM = 6
    KW_END = 7
    KW_REP = 8
    L_PAREN = 9
    R_PAREN = 10
    ARR = 11
    DP = 12
    EQ = 13
    PZ = 14
    SLASH = 15
    ZAP = 16
    ALT = 17
    EOF = 18
