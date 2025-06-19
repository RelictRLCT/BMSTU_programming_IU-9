from collections import defaultdict


class Symbol:
    def __init__(self, ident: str, pos: str):
        self.ident = ident
        self.pos   = pos


class TokenSymbol(Symbol):
    def __init__(self, ident: str, pos: str):
        super().__init__(ident, pos)


class RuleSymbol(Symbol):
    def __init__(self, ident: str, pos: str):
        super().__init__(ident, pos)


class TypeSymbol(Symbol):
    def __init__(self, ident: str, typ: str, pos: str):
        super().__init__(ident, pos)
        self.typ = typ


class MethodSymbol(Symbol):
    def __init__(self,
                 ident: str,
                 return_type: str,
                 param_types: list[tuple[str, bool]],
                 pos: str):
        super().__init__(ident, pos)
        self.return_type = return_type
        self.param_types = param_types
        self.used = False


class SymbolTable:
    def __init__(self, parent: "SymbolTable | None" = None):
        self.parent = parent
        self.symbols: dict[str, list[Symbol]] = defaultdict(list)

    def add(self, sym: Symbol):
        self.symbols[sym.ident].append(sym)

    def lookup(self, ident: str) -> Symbol | None:
        table = self
        while table:
            lst = table.symbols.get(ident)
            if lst:
                return lst[0]
            table = table.parent
        return None
