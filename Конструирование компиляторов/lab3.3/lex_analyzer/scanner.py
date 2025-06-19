import typing
from copy import deepcopy

from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import (
    Token,
    SpecToken, IdentToken
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
                case "$":
                    while self.cur.cp() not in ["\n", "\r\n"]:
                        self.cur.next()
                    continue
                case "(":
                    self.cur.next()
                    token = SpecToken(DomainTag.L_PAREN, start, deepcopy(self.cur))
                    return token
                case ")":
                    self.cur.next()
                    token = SpecToken(DomainTag.R_PAREN, start, deepcopy(self.cur))
                    return token
                case ":":
                    self.cur.next()
                    token = SpecToken(DomainTag.DP, start, deepcopy(self.cur))
                    return token
                case ",":
                    self.cur.next()
                    token = SpecToken(DomainTag.ZAP, start, deepcopy(self.cur))
                    return token
                case ";":
                    self.cur.next()
                    token = SpecToken(DomainTag.PZ, start, deepcopy(self.cur))
                    return token
                case "/":
                    self.cur.next()
                    token = SpecToken(DomainTag.SLASH, start, deepcopy(self.cur))
                    return token
                case "=":
                    self.cur.next()
                    token = SpecToken(DomainTag.EQ, start, deepcopy(self.cur))
                    return token
                case "|":
                    self.cur.next()
                    token = SpecToken(DomainTag.ALT, start, deepcopy(self.cur))
                    return token
                case "[":
                    self.cur.next()
                    if self.cur.cp() == "]":
                        self.cur.next()
                        token = SpecToken(DomainTag.ARR, start, deepcopy(self.cur))
                        return token
                    else:
                        self.compiler.add_message(
                            True, deepcopy(self.cur), "Ожидалось: ]"
                        )
                        self.cur.next()
                case "%":
                    self.cur.next()
                    cur_wrd = self.cur.peek_word(3)
                    if cur_wrd not in ('end', 'rep'):
                        cur_wrd = self.cur.peek_word(5)
                        if cur_wrd not in ('class', 'types', 'axiom'):
                            cur_wrd = self.cur.peek_word(6)
                            if cur_wrd != 'tokens':
                                cur_wrd = self.cur.peek_word(7)
                                if cur_wrd not in ('methods', 'grammar'):
                                    self.compiler.add_message(
                                        True, deepcopy(self.cur),
                                        "Ожидалось одно из ключевых слов: "
                                            "end, rep, class, types, axiom, tokens, "
                                            "methods, grammar"
                                    )
                                    self.cur.next()
                                else:
                                    for i in range(7):
                                        self.cur.next()
                                    if cur_wrd == 'methods':
                                        token = SpecToken(DomainTag.KW_METHODS, start, deepcopy(self.cur))
                                        return token
                                    else:
                                        token = SpecToken(DomainTag.KW_GRAMMAR, start, deepcopy(self.cur))
                                        return token
                            else:
                                for i in range(6):
                                    self.cur.next()
                                token = SpecToken(DomainTag.KW_TOKENS, start, deepcopy(self.cur))
                                return token
                        else:
                            for i in range(5):
                                self.cur.next()
                            if cur_wrd == 'class':
                                token = SpecToken(DomainTag.KW_CLASS, start, deepcopy(self.cur))
                                return token
                            elif cur_wrd == 'types':
                                token = SpecToken(DomainTag.KW_TYPES, start, deepcopy(self.cur))
                                return token
                            else:
                                token = SpecToken(DomainTag.KW_AXIOM, start, deepcopy(self.cur))
                                return token
                    else:
                        for i in range(3):
                            self.cur.next()
                        if cur_wrd == 'end':
                            token = SpecToken(DomainTag.KW_END, start, deepcopy(self.cur))
                            return token
                        else:
                            token = SpecToken(DomainTag.KW_REP, start, deepcopy(self.cur))
                            return token
                case _:
                    if self.cur.isletter():
                        self.cur.next()
                        while self.cur.isletter() or self.cur.isnumber() or self.cur.cp() == '_':
                            self.cur.next()

                        name = self.program[start.index:self.cur.index]
                        token = IdentToken(self.compiler.add_name(name), start, deepcopy(self.cur))
                        return token
                    elif self.cur.cp() == "":
                        return SpecToken(DomainTag.EOF, start, deepcopy(self.cur))
                    else:
                        self.compiler.add_message(
                            True, deepcopy(self.cur), "Неизвестный синтаксис"
                        )
                        self.cur.next()
        return SpecToken(DomainTag.EOF, deepcopy(self.cur), deepcopy(self.cur))
