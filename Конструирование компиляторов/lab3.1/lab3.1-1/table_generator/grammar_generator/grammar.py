from collections import defaultdict

from lex_analyzer.fragment import Fragment
from lex_analyzer.token import Token


class Grammar:
    def __init__(self):
        self.axiom: tuple[str, Fragment] | None = None
        self.rules: dict[tuple[str, Fragment], list[list[str]]] = defaultdict(list)
        self.nterms: set[tuple[str, Fragment]] = set()
        self.terms: set[tuple[str, Fragment]] = set()
        self.terms_set: set[str] = set()
        self.nterms_set: set[str] = set()