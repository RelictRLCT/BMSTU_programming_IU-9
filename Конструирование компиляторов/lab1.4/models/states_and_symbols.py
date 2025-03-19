from enum import Enum


class StatesEnum(Enum):
    START = 0
    IDENT = 1
    DIG = 2

    KEYWORD_D = 3
    KEYWORD_E = 4

    DELETE_L = 5
    DELETE_E = 6
    DELETE_T = 7

    DECLTYPE_C = 8
    DECLTYPE_L = 9
    DECLTYPE_T = 10
    DECLTYPE_Y = 11
    DECLTYPE_P = 12

    KEYWORD = 13

    ARROW_LR = 14
    ARROW_LR_FIN = 15
    ARROW_RL = 16
    ARROW_RL_FIN = 17

    STR_START = 18
    STR_FIN = 19
    SPACE = 20

    ERR = 21


class AlfabetEnum(Enum):
    LET = 0
    DIG = 1
    SPACE = 2
    D = 3
    E = 4
    L = 5
    T = 6
    C = 7
    Y = 8
    P = 9
    TIEF = 10
    LR = 11
    RL = 12
    APOSTROPHE = 13
    SYMBOL = 14
    ERR = 15
