from models.states_and_symbols import AlfabetEnum, StatesEnum


class Automata:
    def __init__(self):
        # START = 0
        # IDENT = 1
        # DIG = 2
        #
        # KEYWORD_D = 3
        # KEYWORD_E = 4
        #
        # DELETE_L = 5
        # DELETE_E = 6
        # DELETE_T = 7
        #
        # DECLTYPE_C = 8
        # DECLTYPE_L = 9
        # DECLTYPE_T = 10
        # DECLTYPE_Y = 11
        # DECLTYPE_P = 12
        #
        # KEYWORD = 13
        #
        # ARROW_LR = 14
        # ARROW_LR_FIN = 15
        # ARROW_RL = 16
        # ARROW_RL_FIN = 17
        #
        # STR_START = 18
        # STR_FIN = 19
        # SPACE = 20
        #
        # ERR = 21

        self.transitions = [
            # LET, DIG, SPACE, D, E, L, T, C, Y, P, THIEF, LR, RL, AP, SYM, ERR
            [1, 2, 20, 3, 1, 1, 1, 1, 1, 1, 14, 21, 16, 18, 21, 21],  # 0 Стартовое
            [1, 1, 21, 1, 1, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 1 IDENT
            [21, 2, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21],  # 2 DIG

            [1, 1, 21, 1, 4, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 3 KEYWORD_D
            [1, 1, 21, 1, 1, 5, 1, 8, 1, 1, 21, 21, 21, 21, 21, 21],  # 4 KEYWORD_E
            [1, 1, 21, 1, 6, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 5 DELETE_L
            [1, 1, 21, 1, 1, 1, 7, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 6 DELETE_E
            [1, 1, 21, 1, 13, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 7 DELETE_T

            [1, 1, 21, 1, 1, 9, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 8 DECLTYPE_C
            [1, 1, 21, 1, 1, 1, 10, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 9 DECLTYPE_L
            [1, 1, 21, 1, 1, 1, 1, 1, 11, 1, 21, 21, 21, 21, 21, 21],  # 10 DECLTYPE_T
            [1, 1, 21, 1, 1, 1, 1, 1, 1, 12, 21, 21, 21, 21, 21, 21],  # 11 DECLTYPE_Y
            [1, 1, 21, 1, 13, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 12 DECLTYPE_P
            [1, 1, 21, 1, 1, 1, 1, 1, 1, 1, 21, 21, 21, 21, 21, 21],  # 13 KEYWORD

            [21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 15, 21, 21, 21, 21],  # 14 ARROW_LR
            [21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21],  # 15 ARROW_LR_FIN
            [21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 17, 21, 21, 21, 21, 21],  # 16 ARROW_RL
            [21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21],  # 17 ARROW_RL_FIN

            [18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 18, 18],  # 18 STR_START
            [21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 18, 21, 21],  # 19 STR_FIN
            [21, 21, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21],  # 20 SPACE
        ]
        self.finals = [1, 2, 13, 15, 17, 19, 20]
        self.current_state = StatesEnum.START.value

    @staticmethod
    def get_sym_code(symbol: str) -> int:
        match symbol:
            case "d":
                return AlfabetEnum.D.value
            case "e":
                return AlfabetEnum.E.value
            case "l":
                return AlfabetEnum.L.value
            case "t":
                return AlfabetEnum.T.value
            case "c":
                return AlfabetEnum.C.value
            case "y":
                return AlfabetEnum.Y.value
            case "p":
                return AlfabetEnum.P.value
            case "-":
                return AlfabetEnum.TIEF.value
            case ">":
                return AlfabetEnum.LR.value
            case "<":
                return AlfabetEnum.RL.value
            case "'":
                return AlfabetEnum.APOSTROPHE.value
            case _ if symbol.isspace():
                return AlfabetEnum.SPACE.value
            case _ if symbol.isalpha():
                return AlfabetEnum.LET.value
            case _ if symbol.isdigit():
                return AlfabetEnum.DIG.value
            case _:
                return AlfabetEnum.ERR.value

    def next_state(self, symbol: str) -> int:
        self.current_state = self.transitions[self.current_state][
            self.get_sym_code(symbol)
        ]
        return self.current_state

    def is_final(self, state: int) -> bool:
        return state in self.finals
