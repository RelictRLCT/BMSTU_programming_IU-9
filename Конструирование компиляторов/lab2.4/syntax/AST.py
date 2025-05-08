from dataclasses import dataclass


# Class ::= %class IDENT
@dataclass
class Class:
    name: str


# Tokens ::= %tokens IDENT+
@dataclass
class Tokens:
    tokens: list[str]


# Types ::= %types (IDENT (, IDENT)* : IDENT;)+
@dataclass
class Types:
    types: dict[str:str]


# Args ::= (IDENT([])? (, IDENT([])?)*)?
@dataclass
class Args:
    args: list[tuple[str, bool]] # True => массив


# Methods ::= %methods (IDENT IDENT(Args);)+
@dataclass
class Methods:
    methods: list[tuple[str, str, Args]]


@dataclass
class Alternates:
    pass


# AltElement ::= (%rep)? (IDENT | (Alternates))
@dataclass
class AltElement:
    element: str | Alternates
    rep: bool = False


# Alternates ::= (AltElement* (/ IDENT)?)*
@dataclass
class Alternates:
    alternates: list[tuple[list[AltElement], str | None]] # str | None -- вызов функции в конце


# Grammar ::= %grammar (IDENT = Alternates;)+
@dataclass
class Grammar:
    rules: dict[str:Alternates]


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
