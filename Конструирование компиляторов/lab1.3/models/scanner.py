import typing
from copy import deepcopy

from models.domain_tags import DomainTag
from models.token import (
    Token,
    SpecToken,
    IdentToken,
    IntToken,
    IntDoubleToken,
    DirectToken,
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

    def next_token(self) -> Token:
        while self.cur.cp() != "":
            while self.cur.iswhitespace():
                self.cur.next()

            start: Position = deepcopy(self.cur)
            match self.cur.cp():
                case "(":
                    token = SpecToken(DomainTag.LPAREN, start, self.cur)
                    self.cur.next()
                    return token
                case ")":
                    token = SpecToken(DomainTag.RPAREN, start, self.cur)
                    self.cur.next()
                    return token
                case "<":
                    token = SpecToken(DomainTag.LPARENTR, start, self.cur)
                    self.cur.next()
                    return token
                case ">":
                    token = SpecToken(DomainTag.RPARENTR, start, self.cur)
                    self.cur.next()
                    return token
                case _:
                    if self.cur.isletter():
                        if self.cur.cp().isupper():
                            name = ""
                            while self.cur.is_letter_digit_or_thief():
                                name += self.cur.cp()
                                self.cur.next()
                            return IdentToken(
                                self.compiler.add_name(name), start, self.cur
                            )
                        else:
                            self.compiler.add_message(
                                True,
                                deepcopy(self.cur),
                                "Идентификатор должен начинаться с большой буквы",
                            )
                            self.cur.next()
                    elif self.cur.isnumber():
                        val = 0
                        accuracy = False
                        if self.cur.cp() == "0":
                            self.cur.next()
                            if self.cur.isnumber():
                                accuracy = True
                            else:
                                return IntToken(0, start, self.cur)
                        val = 10 * val + ord(self.cur.cp()) - ord("0")
                        self.cur.next()
                        while self.cur.isnumber():
                            val = 10 * val + ord(self.cur.cp()) - ord("0")
                            self.cur.next()
                        if self.cur.isletter():
                            self.compiler.add_message(
                                False, deepcopy(self.cur), "Нужен разделитель"
                            )
                        if accuracy:
                            return IntDoubleToken(val, start, self.cur)
                        return IntToken(val, start, self.cur)
                    elif self.cur.isvalut():
                        correctdir = False
                        self.cur.next()
                        name = ""
                        while self.cur.cp().isupper():
                            name += self.cur.cp()
                            correctdir = True
                            self.cur.next()
                        if correctdir:
                            return DirectToken(
                                name,
                                start,
                                deepcopy(self.cur),
                            )
                        else:
                            self.compiler.add_message(
                                True, deepcopy(self.cur), "Пустая директива"
                            )
                            self.cur.next()
                    elif self.cur.cp() == "":
                        return SpecToken(DomainTag.EOF, start, self.cur)
                    else:
                        self.compiler.add_message(
                            True, deepcopy(self.cur), "Неизвестный синтаксис"
                        )
                        self.cur.next()
        return SpecToken(DomainTag.EOF, deepcopy(self.cur), deepcopy(self.cur))
