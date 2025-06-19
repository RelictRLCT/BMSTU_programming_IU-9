import typing
from copy import deepcopy

from lex_analyzer.token import (
    Token,
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
                    token = Token("lqparen", '', start, deepcopy(self.cur))
                    return token
                case "]":
                    self.cur.next()
                    token = Token("rqparen", '', start, deepcopy(self.cur))
                    return token
                case _:
                    if self.cur.isletter():
                        if self.cur.peek_word(5) == 'axiom':
                            for i in range(4):
                                self.cur.next()
                            self.cur.next()
                            token = Token('ax', '', start, deepcopy(self.cur))
                            return token
                        elif self.cur.cp().isupper():
                            nterm = self.cur.cp()
                            self.cur.next()
                            while self.cur.isletter():
                                nterm += self.cur.cp()
                                self.cur.next()
                            if self.cur.cp() == "'":
                                nterm += self.cur.cp()
                                self.cur.next()
                            token = Token('nt', nterm, start, deepcopy(self.cur))
                            return token
                        else:
                            term = self.cur.cp()
                            self.cur.next()
                            while self.cur.isletter():
                                term += self.cur.cp()
                                self.cur.next()
                            token = Token('t', term, start, deepcopy(self.cur))
                            return token
                    elif self.cur.cp() in ["+", "-", "/", "*", "(", ")"]:
                        attr = self.cur.cp()
                        self.cur.next()
                        token = Token('t', attr, start, deepcopy(self.cur))
                        return token
                    elif self.cur.cp() == "":
                        return Token('EOF', '', start, deepcopy(self.cur))
                    else:
                        self.compiler.add_message(
                            True, deepcopy(self.cur), "Неизвестный синтаксис"
                        )
                        self.cur.next()
        return Token('EOF', '', deepcopy(self.cur), deepcopy(self.cur))
