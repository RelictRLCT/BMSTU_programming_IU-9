from dataclasses import dataclass
from typing import Optional

from semantic.errors import TokenRule, RepeatType, RepeatRule, RepeatMethod, UndefMethod, UnusedMethod, \
    MethodTypeMismatch, MoreThenOneType, AlternateTypeMismatch, UndefNonterminal, UntypedNonterminal
from semantic.symbol_table import SymbolTable, TokenSymbol, RuleSymbol, TypeSymbol, MethodSymbol


# Class ::= %class IDENT
@dataclass
class Class:
    name: str


# Tokens ::= %tokens IDENT+
@dataclass
class Tokens:
    tokens: list[str]
    symtab: SymbolTable

    def build_symtab(self, pos):
        for i in range(len(self.tokens)):
            self.symtab.add(TokenSymbol(self.tokens[i], pos[i]))


# Types ::= %types (IDENT (, IDENT)* : IDENT;)+
@dataclass
class Types:
    types: list[tuple[str, str]]
    symtab: SymbolTable

    def build_symtab(self, positions: list[str]):
        for type, pos in zip(self.types, positions):
            self.symtab.add(TypeSymbol(type[0], type[1], pos))

    def check(self):
        for ident, syms in self.symtab.symbols.items():
            if len(syms) > 1:
                raise RepeatType(syms[1].pos, ident)


# Args ::= (IDENT([])? (, IDENT([])?)*)?
@dataclass
class Args:
    args: list[tuple[str, bool]] # True => массив


# Methods ::= %methods (IDENT IDENT(Args);)+
@dataclass
class Methods:
    methods: list[tuple[str, str, Args]]
    symtab: SymbolTable

    def build_symtab(self, positions: list[str]):
        for method, pos in zip(self.methods, positions):
            self.symtab.add(MethodSymbol(method[1], method[0], method[2].args, pos))

    def check(self):
        for ident, syms in self.symtab.symbols.items():
            if len(syms) > 1:
                raise RepeatMethod(syms[1].pos, ident)


@dataclass
class Alternates:
    pass


# AltElement ::= (%rep)? (IDENT | (Alternates))
@dataclass
class AltElement:
    element: str | Alternates
    rep: bool = False
    type: Optional[str] = None


# Alternates ::= (AltElement* (/ IDENT)?)*
@dataclass
class Alternates:
    alternates: list[tuple[list[AltElement], str | None]] # str | None -- вызов функции в конце
    alt_type: Optional[str] = None
    rep: bool = False


# Grammar ::= %grammar (IDENT = Alternates;)+
@dataclass
class Grammar:
    rules: list[tuple[str, Alternates]]
    symtab: SymbolTable
    tokens_symtab: SymbolTable
    types_symtab: SymbolTable
    methods_symtab: SymbolTable

    def build_symtab(self, positions: list[str]):
        for rule, pos in zip(self.rules, positions):
            self.symtab.add(RuleSymbol(rule[0], pos))

    def check_rules_have_type(self):
        for rule_name, _ in self.rules:
            if self.types_symtab.lookup(rule_name) is None:
                pos = self.symtab.lookup(rule_name).pos
                raise UntypedNonterminal(pos, rule_name)

    def check_nonterminals_defined(self):
        defined_nt = {name for name, _ in self.rules}
        terminals = set(self.tokens_symtab.symbols.keys())

        def visit(alts: Alternates, rule_pos: str):
            for elems, _ in alts.alternates:
                for el in elems:
                    if isinstance(el.element, str):
                        name = el.element
                        if name not in terminals and name not in defined_nt:
                            raise UndefNonterminal(rule_pos, name)
                    else:
                        visit(el.element, rule_pos)

        for rule_name, alts in self.rules:
            rule_pos = self.symtab.lookup(rule_name).pos
            visit(alts, rule_pos)

    def annotate_element_types(self, alts: Alternates):
        for elems, _ in alts.alternates:
            for el in elems:
                if isinstance(el.element, str):
                    tsym = self.types_symtab.lookup(el.element)
                    el.type = tsym.typ if isinstance(tsym, TypeSymbol) else None
                else:
                    self.annotate_element_types(el.element)

    def compute_alts_type(self, rule_name: str, alts: Alternates, rule_pos: str):
        for elems, _ in alts.alternates:
            for el in elems:
                if isinstance(el.element, Alternates):
                    self.compute_alts_type(rule_name, el.element, rule_pos)

        alt_types = []
        for elems, method_call in alts.alternates:
            if method_call:
                m_sym = self.methods_symtab.lookup(method_call)

                # (type, rep)
                actual = []
                for el in elems:
                    if isinstance(el.element, Alternates):
                        base = el.element.alt_type
                    else:
                        base = el.type
                    if base is not None:
                        actual.append((base, el.rep))

                if actual != m_sym.param_types:
                    raise MethodTypeMismatch(
                        rule_pos,
                        m_sym.ident
                    )
                # тип у альтернативы, как у метода
                alt_ty = m_sym.return_type

            else:
                # без метода: не более одного типизированного элемента
                typed = [(el.type, el.rep) for el in elems if el.type is not None]
                if len(typed) > 1:
                    raise MoreThenOneType(
                        rule_pos,
                        rule_name
                    )

                if not typed:
                    alt_ty = None
                else:
                    base, _ = typed[0]
                    alt_ty = base

            alt_types.append(alt_ty)

        # все alt_types должны совпадать
        unique = set(alt_types)
        if len(unique) > 1:
            raise AlternateTypeMismatch(
                rule_pos,
                rule_name
            )

        alts.alt_type = unique.pop() if unique else None

    def annotate_all(self):
        for _, alts in self.rules:
            self.annotate_element_types(alts)

    def mark_methods_used(self, alternates: Alternates, rule_pos: str):
        for alt_elems, method_call in alternates.alternates:
            if method_call is not None:
                m_sym = self.methods_symtab.lookup(method_call)
                if m_sym is None:
                    raise UndefMethod(rule_pos, method_call)
                m_sym.used = True

            for elem in alt_elems:
                if isinstance(elem.element, Alternates):
                    self.mark_methods_used(elem.element, rule_pos)

    def check(self):
        # повторы и токены в правилах
        for ident, syms in self.symtab.symbols.items():
            if len(syms) > 1:
                raise RepeatRule(syms[1].pos, ident)

            if self.tokens_symtab.lookup(ident):
                raise TokenRule(syms[0].pos, ident)

        self.check_nonterminals_defined()
        self.check_rules_have_type()

        # undef
        for rule_name, alternates in self.rules:
            rule_pos = self.symtab.lookup(rule_name).pos
            self.mark_methods_used(alternates, rule_pos)

        # unused
        for syms in self.methods_symtab.symbols.values():
            m_sym = syms[0]
            if isinstance(m_sym, MethodSymbol) and not m_sym.used:
                raise UnusedMethod(m_sym.pos, m_sym.ident)

        self.annotate_all()
        for rule_name, alts in self.rules:
            rule_pos = self.symtab.lookup(rule_name).pos
            self.compute_alts_type(rule_name, alts, rule_pos)


# Axiom ::= %axiom IDENT
@dataclass
class Axiom:
    ax: str


# Prog ::= Class Tokens Types Methods Grammar Axiom End
@dataclass
class Prog:
    class_: Class
    tokens: Tokens
    types: Types
    methods: Methods
    grammar: Grammar
    axiom: Axiom

    def check(self):
        self.types.check()
        self.methods.check()
        self.grammar.check()
