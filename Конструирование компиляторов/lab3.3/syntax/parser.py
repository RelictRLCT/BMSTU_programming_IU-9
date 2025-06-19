from copy import deepcopy
from crypt import methods
from lib2to3.fixes.fix_imports import alternates

from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import Token
from lex_analyzer.сompiler import Compiler
from semantic.errors import SemanticError
from semantic.symbol_table import SymbolTable, MethodSymbol
from syntax.AST import Prog, Class, Tokens, Types, Args, Methods, AltElement, Alternates, Grammar, Axiom
from syntax.print_symtabs import print_symtabs

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
        tokens, positions = [], []
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            tokens.append(compiler.get_name(Sym.code))
            positions.append(Sym.coords.tostring())
            next_tok()
            while DomainTag(Sym.tag) == DomainTag.IDENT:
                tokens.append(compiler.get_name(Sym.code))
                positions.append(Sym.coords.tostring())
                next_tok()

            tokens_node = Tokens(tokens, SymbolTable())
            tokens_node.build_symtab(positions)
            return tokens_node
        else:
            err(Sym, 'Tokens(): Ожидался хотя бы один токен')
    else:
        err(Sym, 'Ожидалось объявление блока токенов')


# Types ::= %types (IDENT (, IDENT)* : IDENT;)+
def Types_parse(compiler: Compiler) -> Types:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_TYPES:
        next_tok()
        types_list = []
        positions = []
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            names = [compiler.get_name(Sym.code)]
            positions.append(Sym.coords.tostring())
            next_tok()
            while DomainTag(Sym.tag) == DomainTag.ZAP:
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.IDENT:
                    names.append(compiler.get_name(Sym.code))
                    positions.append(Sym.coords.tostring())
                    next_tok()
                else:
                    err(Sym, 'Types(): Ожидался идентификатор')
            if DomainTag(Sym.tag) == DomainTag.DP:
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.IDENT:
                    for name in names:
                        types_list.append((name, compiler.get_name(Sym.code)))
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
                positions.append(Sym.coords.tostring())
                next_tok()
                while DomainTag(Sym.tag) == DomainTag.ZAP:
                    next_tok()
                    if DomainTag(Sym.tag) == DomainTag.IDENT:
                        names.append(compiler.get_name(Sym.code))
                        positions.append(Sym.coords.tostring())
                        next_tok()
                    else:
                        err(Sym, 'Types(): Ожидался идентификатор')
                if DomainTag(Sym.tag) == DomainTag.DP:
                    next_tok()
                    if DomainTag(Sym.tag) == DomainTag.IDENT:
                        for name in names:
                            types_list.append((name, compiler.get_name(Sym.code)))
                        next_tok()
                        if DomainTag(Sym.tag) == DomainTag.PZ:
                            next_tok()
                        else:
                            err(Sym, 'Types(): Ожидалось ;')
                    else:
                        err(Sym, 'Types(): Ожидался идентификатор')
                else:
                    err(Sym, 'Types(): Ожидалось двоеточие')

            types_node = Types(types_list, SymbolTable())
            types_node.build_symtab(positions)
            return types_node
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
    return Args([])


# Methods ::= %methods (IDENT IDENT(Args);)+
def Methods_parse(compiler: Compiler, types_table: SymbolTable) -> Methods:
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_METHODS:
        next_tok()
        methods: list[tuple[str, str, Args]] = []
        positions = []
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            tp = compiler.get_name(Sym.code)
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                positions.append(Sym.coords.tostring())
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
                positions.append(Sym.coords.tostring())
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

        methods_node = Methods(methods, SymbolTable(parent=types_table))
        methods_node.build_symtab(positions)
        return methods_node

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
def Grammar_parse(compiler: Compiler, methods_symtab: SymbolTable, tokens_symtab: SymbolTable, types_symtab: SymbolTable):
    global Sym
    if DomainTag(Sym.tag) == DomainTag.KW_GRAMMAR:
        rules: list[tuple[str, Alternates]] = []
        next_tok()
        positions = []
        if DomainTag(Sym.tag) == DomainTag.IDENT:
            name = compiler.get_name(Sym.code)
            positions.append(Sym.coords.tostring())
            next_tok()
            if DomainTag(Sym.tag) == DomainTag.EQ:
                next_tok()
                alternates_ = Alternates_parse(compiler)
                if DomainTag(Sym.tag) == DomainTag.PZ:
                    rules.append((name, alternates_))
                    next_tok()
                else:
                    err(Sym, 'Grammar(): Ожидалось ;')
            else:
                err(Sym, 'Grammar(): Ожидалось =')

            while DomainTag(Sym.tag) == DomainTag.IDENT:
                name = compiler.get_name(Sym.code)
                positions.append(Sym.coords.tostring())
                next_tok()
                if DomainTag(Sym.tag) == DomainTag.EQ:
                    next_tok()
                    alternates_ = Alternates_parse(compiler)
                    if DomainTag(Sym.tag) == DomainTag.PZ:
                        rules.append((name, alternates_))
                        next_tok()
                    else:
                        err(Sym, 'Grammar(): Ожидалось ;')
                else:
                    err(Sym, 'Grammar(): Ожидалось =')

        grammar_node = Grammar(rules, SymbolTable(), tokens_symtab, types_symtab, methods_symtab)
        grammar_node.build_symtab(positions)
        return grammar_node

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
    methods = Methods_parse(compiler, types.symtab)
    rules = Grammar_parse(compiler, methods.symtab, tokens.symtab, types.symtab)
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

    try:
        prog.check()
    except SemanticError as e:
        print(e.message)
        exit(1)

    # print_symtabs(prog)

    print('\nПрограмма корректна')

    return prog
