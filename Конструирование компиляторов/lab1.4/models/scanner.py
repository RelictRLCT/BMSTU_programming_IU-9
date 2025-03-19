import typing
from copy import deepcopy

from models.automata import Automata
from models.domain_tags import DomainTag
from models.states_and_symbols import StatesEnum
from models.token import (
    Token,
    SpecToken,
    IdentToken,
    SpaceToken,
    KeywordToken,
    DigToken, ArrowLRToken, ArrowRLToken, StringToken,
)

if typing.TYPE_CHECKING:
    from models.сompiler import Compiler
from models.fragment import Fragment
from models.position import Position


class Scanner:
    def __init__(self, program: str, compiler: "Compiler"):
        self.program = program
        self.compiler = compiler
        self.cur = Position(program)
        self.comments: list[Fragment] = []
        self.automata = Automata()

    def next_token(self) -> Token:
        last_final = 0
        while self.cur.cp() != "":
            start: Position = deepcopy(self.cur)

            following_final = deepcopy(self.cur)
            while (
                self.cur.cp() != ""
                and self.automata.current_state != StatesEnum.ERR.value
            ):
                self.automata.next_state(self.cur.cp())
                if self.automata.is_final(self.automata.current_state):
                    last_final = self.automata.current_state
                    following_final = deepcopy(self.cur)
                self.cur.next()

            if self.automata.current_state == StatesEnum.ERR.value:
                if last_final != 0:
                    match last_final:
                        case StatesEnum.IDENT.value | StatesEnum.KEYWORD_D.value |\
                             StatesEnum.KEYWORD_E.value | StatesEnum.DELETE_E.value |\
                             StatesEnum.DELETE_L.value | StatesEnum.DELETE_T.value |\
                             StatesEnum.DECLTYPE_C.value | StatesEnum.DECLTYPE_T.value |\
                             StatesEnum.DECLTYPE_Y.value | StatesEnum.DECLTYPE_P.value:
                            token = IdentToken(
                                self.compiler.add_name(
                                    self.program[start.index : self.cur.index - 1]
                                ),
                                start,
                                following_final,
                            )
                        case StatesEnum.SPACE.value:
                            token = SpaceToken(start, following_final)
                        case StatesEnum.KEYWORD.value:
                            token = KeywordToken(
                                self.program[start.index : self.cur.index - 1],
                                start,
                                following_final,
                            )
                        case StatesEnum.DIG.value:
                            token = DigToken(
                                int(self.program[start.index : self.cur.index - 1]),
                                start,
                                following_final,
                            )
                        case StatesEnum.ARROW_LR_FIN.value:
                            token = ArrowLRToken(
                                start,
                                following_final,
                            )
                        case StatesEnum.ARROW_RL_FIN.value:
                            token = ArrowRLToken(
                                start,
                                following_final,
                            )
                        case StatesEnum.STR_FIN.value:
                            token = StringToken(
                                self.program[start.index: self.cur.index - 1],
                                start,
                                following_final,
                            )
                    following_final.next()
                    self.cur = deepcopy(following_final)
                    self.automata.current_state = StatesEnum.START.value
                    return token
                else:
                    self.compiler.add_message(
                        True,
                        deepcopy(start),
                        "Ошибка в синтаксисе! ",
                    )
                    start.next()
                    self.cur = start
                    self.automata.current_state = StatesEnum.START.value
                    continue

        return SpecToken(DomainTag.EOF, deepcopy(self.cur), deepcopy(self.cur))
