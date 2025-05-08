import typing
from copy import deepcopy
from telnetlib import STATUS

from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import (
    Token,
    SpecToken, TermToken, NTermToken
)

if typing.TYPE_CHECKING:
    from lex_analyzer.сompiler import Compiler
from lex_analyzer.fragment import Fragment
from lex_analyzer.position import Position


class Scanner:
    def __init__(self, program: str, compiler: "Compiler"):
        self.program = program
        self.compiler = compiler
        self.cur = Position(program)
        self.comments: list[Fragment] = []

    def next_token(self) -> Token:
        while self.cur.cp() != "":
            while self.cur.iswhitespace():
                self.cur.next()

            start: Position = deepcopy(self.cur)
            match self.cur.cp():
                case "%":
                    while self.cur.cp() not in ["\n", "\r\n"]:
                        self.cur.next()
                    continue
                case "[":
                    self.cur.next()
                    token = SpecToken(DomainTag.L_Q_PAREN, start, deepcopy(self.cur))
                    return token
                case "]":
                    self.cur.next()
                    token = SpecToken(DomainTag.R_Q_PAREN, start, deepcopy(self.cur))
                    return token
                case _:
                    if self.cur.isletter():
                        if self.cur.peek_word(5) == 'axiom':
                            for i in range(4):
                                self.cur.next()
                            self.cur.next()
                            token = SpecToken(DomainTag.AXIOM, start, deepcopy(self.cur))
                            return token
                        elif self.cur.cp().isupper():
                            nterm = self.cur.cp()
                            if self.cur.peek_word(2)[1:] == "'":
                                self.cur.next()
                                nterm += self.cur.cp()
                            self.cur.next()
                            token = NTermToken(nterm, start, deepcopy(self.cur))
                            return token
                        else:
                            attr = self.cur.cp()
                            self.cur.next()
                            token = TermToken(attr, start, deepcopy(self.cur))
                            return token
                    elif self.cur.cp() in ["+", "-", "/", "*", "(", ")"]:
                        attr = self.cur.cp()
                        self.cur.next()
                        token = TermToken(attr, start, deepcopy(self.cur))
                        return token
                    elif self.cur.cp() == "":
                        return SpecToken(DomainTag.EOF, start, deepcopy(self.cur))
                    else:
                        self.compiler.add_message(
                            True, deepcopy(self.cur), "Неизвестный синтаксис"
                        )
                        self.cur.next()
        return SpecToken(DomainTag.EOF, deepcopy(self.cur), deepcopy(self.cur))
