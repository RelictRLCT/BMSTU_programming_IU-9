import enum


class DomainTag(enum.Enum):
    TERM = 0
    NTERM = 1
    L_Q_PAREN = 2
    R_Q_PAREN = 3
    AXIOM = 4
    EOF = 5
