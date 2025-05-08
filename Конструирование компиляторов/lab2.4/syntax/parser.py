from lib2to3.fixes.fix_imports import alternates

from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import Token
from lex_analyzer.сompiler import Compiler
from syntax.AST import Prog, Class, Tokens, Types, Args, Methods, AltElement, Alternates, Grammar, Axiom

Sym: Token
Tokens_list: list[Token] = []
i = 0


def err(sym: Token, info: str):
    print(f'Синтаксическая ошибка на позиции {sym.coords.tostring()}. {info}')
    exit(1)


def next_tok():
    global i
    global Tokens_list
    global Sym
    Sym = Tokens_list[i]
    i += 1


# Class ::= %class IDENT
def Class_parse(compiler: Compiler) -> Class:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_CLASS:
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            name = compiler.get_name(Sym.code)
            next_tok()
            return Class(name)
        else:
            err(Sym, 'Ожидалось название класса')
    else:
        err(Sym, 'Ожидалось объявление класса')


# Tokens ::= %tokens IDENT+
def Tokens_parse(compiler: Compiler) -> Tokens:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_TOKENS:
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            tokens = [compiler.get_name(Sym.code)]
            next_tok()
            while DomainTag(Sym.tag) == DomainTag.IDENT:
                tokens.append(compiler.get_name(Sym.code))
                next_tok()
            return Tokens(tokens)
        else:
            err(Sym, 'Tokens(): Ожидался хотя бы один токен')
    else:
        err(Sym, 'Ожидалось объявление блока токенов')


# Types ::= %types (IDENT (, IDENT)* : IDENT;)+
def Types_parse(compiler: Compiler) -> Types:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_TYPES:
        next_tok()
        types_dict = {}
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            names = [compiler.get_name(Sym.code)]
            next_tok()
            while DomainTag(Sym.tag) == DomainTag.ZAP:
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.IDENT:
                    names.append(compiler.get_name(Sym.code))
                    next_tok()
                else:
                    err(Sym, 'Types(): Ожидался идентификатор')
            if DomainTag(Sym.tag) == DomainTag.DP:
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.IDENT:
                    for name in names:
                        types_dict[name] = compiler.get_name(Sym.code)
                    next_tok()
                    if DomainTag(Sym.tag) == DomainTag.PZ:
                        next_tok()
                    else:
                        err(Sym, 'Types(): Ожидалось ;')
                else:
                    err(Sym, 'Types(): Ожидался идентификатор')
            else:
                err(Sym, 'Types(): Ожидалось двоеточие')

            while DomainTag(Sym.tag) == DomainTag.IDENT:
                names = [compiler.get_name(Sym.code)]
                next_tok()
                while DomainTag(Sym.tag) == DomainTag.ZAP:
                    next_tok()
                    if DomainTag(Sym.tag) == DomainTag.IDENT:
                        names.append(compiler.get_name(Sym.code))
                        next_tok()
                    else:
                        err(Sym, 'Types(): Ожидался идентификатор')
                if DomainTag(Sym.tag) == DomainTag.DP:
                    next_tok()
                    if DomainTag(Sym.tag) == DomainTag.IDENT:
                        for name in names:
                            types_dict[name] = compiler.get_name(Sym.code)
                        next_tok()
                        if DomainTag(Sym.tag) == DomainTag.PZ:
                            next_tok()
                        else:
                            err(Sym, 'Types(): Ожидалось ;')
                    else:
                        err(Sym, 'Types(): Ожидался идентификатор')
                else:
                    err(Sym, 'Types(): Ожидалось двоеточие')
            return Types(types_dict)
    else:
        err(Sym, 'Ожидалось объявление блока типов')


# Args ::= (IDENT([])? (, IDENT([])?)*)?
def Args_parse(compiler: Compiler) -> Args:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.IDENT:
        name = compiler.get_name(Sym.code)
        args: list[tuple[str, bool]] = []
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.ARR:
            args.append((name, True))
            next_tok()
        else:
            args.append((name, False))

        while DomainTag(Sym.tag) == DomainTag.ZAP:
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.ARR:
                    args.append((name, True))
                    next_tok()
                else:
                    args.append((name, False))
            else:
                err(Sym, 'Args(): Ожидался идентификатор')
        return Args(args)


# Methods ::= %methods (IDENT IDENT(Args);)+
def Methods_parse(compiler: Compiler) -> Methods:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_METHODS:
        next_tok()
        methods: list[tuple[str, str, Args]] = []
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            tp = compiler.get_name(Sym.code)
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.L_PAREN:
                    next_tok()
                    args = Args_parse(compiler)
                    if DomainTag(Sym.tag) == DomainTag.R_PAREN:
                        next_tok()
                        if DomainTag(Sym.tag) == DomainTag.PZ:
                            methods.append((tp, name, args))
                            next_tok()
                        else:
                            err(Sym, 'Methods(): Ожидалось ;')
                    else:
                        err(Sym, 'Methods(): Ожидалось )')
                else:
                    err(Sym, 'Methods(): Ожидалось (')
            else:
                err(Sym, 'Methods(): Ожидался идентификатор')
        else:
            err(Sym, 'Methods(): Ожидался идентификатор')

        while DomainTag(Sym.tag) == DomainTag.IDENT:
            tp = compiler.get_name(Sym.code)
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.L_PAREN:
                    next_tok()
                    args = Args_parse(compiler)
                    if DomainTag(Sym.tag) == DomainTag.R_PAREN:
                        next_tok()
                        if DomainTag(Sym.tag) == DomainTag.PZ:
                            methods.append((tp, name, args))
                            next_tok()
                        else:
                            err(Sym, 'Methods(): Ожидалось ;')
                    else:
                        err(Sym, 'Methods(): Ожидалось )')
                else:
                    err(Sym, 'Methods(): Ожидалось (')
            else:
                err(Sym, 'Methods(): Ожидался идентификатор ')
        return Methods(methods)

    else:
        err(Sym, 'Ожидался объявление блока методов')


# AltElement ::= (%rep)? (IDENT | (Alternates))
def AltElement_parse(compiler: Compiler) -> AltElement:
    global Sym
    rep_status = False
    if DomainTag(Sym.tag) == DomainTag.KW_REP:
        rep_status = True
        next_tok()
    ret: str | Alternates
    if DomainTag(Sym.tag) == DomainTag.IDENT:
        ret = compiler.get_name(Sym.code)
        next_tok()
    elif DomainTag(Sym.tag) == DomainTag.L_PAREN:
        next_tok()
        ret = Alternates_parse(compiler)
        if DomainTag(Sym.tag) == DomainTag.R_PAREN:
            next_tok()
        else:
            err(Sym, 'AltElement(): Ожидалось )')
    else:
        err(Sym, 'AltElement(): Ожидался идентификатор или (')
        ret = "ERROR"
    return AltElement(ret, rep_status)


# Alternates ::= (AltElement* (/ IDENT)?)*
def Alternates_parse(compiler: Compiler) -> Alternates:
    global Sym
    alt_elements: list[AltElement] = []
    alternates_list: list[tuple[list[AltElement], str | None]] = []
    while DomainTag(Sym.tag) in (DomainTag.IDENT, DomainTag.KW_REP, DomainTag.L_PAREN):
        alt_el = AltElement_parse(compiler)
        alt_elements.append(alt_el)
    if DomainTag(Sym.tag) == DomainTag.SLASH:
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            alternates_list.append((alt_elements, compiler.get_name(Sym.code)))
            next_tok()
        else:
            err(Sym, 'Alternates(): Ожидался идентификатор')
    else:
        alternates_list.append((alt_elements, None))

    alt_elements = []

    while DomainTag(Sym.tag) == DomainTag.ALT:
        next_tok()
        while DomainTag(Sym.tag) in (DomainTag.IDENT, DomainTag.KW_REP, DomainTag.L_PAREN):
            alt_el = AltElement_parse(compiler)
            alt_elements.append(alt_el)
        if DomainTag(Sym.tag) == DomainTag.SLASH:
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.IDENT:
                alternates_list.append((alt_elements, compiler.get_name(Sym.code)))
                next_tok()
            else:
                err(Sym, 'Alternates(): Ожидался идентификатор')
        else:
            alternates_list.append((alt_elements, None))
        alt_elements = []
    return Alternates(alternates_list)


# Grammar ::= %grammar (IDENT = Alternates;)+
def Grammar_parse(compiler: Compiler):
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_GRAMMAR:
        rules: dict[str:Alternates] = {}
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            name = compiler.get_name(Sym.code)
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.EQ:
                next_tok()
                alternates_ = Alternates_parse(compiler)
                if DomainTag(Sym.tag) == DomainTag.PZ:
                    rules[name] = alternates_
                    next_tok()
                else:
                    err(Sym, 'Grammar(): Ожидалось ;')
            else:
                err(Sym, 'Grammar(): Ожидалось =')

            while DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.EQ:
                    next_tok()
                    alternates_ = Alternates_parse(compiler)
                    if DomainTag(Sym.tag) == DomainTag.PZ:
                        rules[name] = alternates_
                        next_tok()
                    else:
                        err(Sym, 'Grammar(): Ожидалось ;')
                else:
                    err(Sym, 'Grammar(): Ожидалось =')
        return Grammar(rules)

    else:
        err(Sym, 'Ожидалось объявление блока правил')


# Axiom ::= %axiom IDENT
def Axiom_parse(compiler: Compiler) -> Axiom:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_AXIOM:
        next_tok()
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            axiom = compiler.get_name(Sym.code)
            next_tok()
        else:
            err(Sym, 'Axiom(): Ожидался идентификатор')
        return Axiom(axiom)
    else:
        err(Sym, 'Ожидалось объявление аксиомы')


def End_parse():
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_END:
        next_tok()
    else:
        err(Sym, 'Ожидалось %end')


# Prog ::= Class Tokens Types Methods Grammar Axiom End
def Prog_parse(compiler: Compiler) -> Prog:
    class_ = Class_parse(compiler)
    tokens = Tokens_parse(compiler)
    types = Types_parse(compiler)
    methods = Methods_parse(compiler)
    rules = Grammar_parse(compiler)
    axiom = Axiom_parse(compiler)
    End_parse()
    prog = Prog(class_, tokens, types, methods, rules, axiom)
    return prog


def rec_down(u: list[Token], compiler: Compiler) -> Prog:
    global Tokens_list
    global Sym
    Tokens_list = u
    next_tok()
    prog = Prog_parse(compiler)
    if DomainTag(Sym.tag) != DomainTag.EOF:
        print('\nERROR')
        exit(1)
    return prog
