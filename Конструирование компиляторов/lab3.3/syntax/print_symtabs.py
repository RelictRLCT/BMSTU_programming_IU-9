from semantic.symbol_table import MethodSymbol, TypeSymbol
from syntax.AST import Prog


def print_symtabs(prog: Prog):
    tables = [
        ("Tokens",  prog.tokens.symtab),
        ("Methods", prog.methods.symtab),
        ("Grammar", prog.grammar.symtab),
        ("Types", prog.types.symtab),
    ]

    for title, st in tables:
        print(f"\n=== {title} ===")
        for name, syms in st.symbols.items():
            for sym in syms:
                line = f"{name}  [{sym.__class__.__name__}], позиция {sym.pos}"
                if isinstance(sym, MethodSymbol):
                    line += f", returns={sym.return_type}, params={sym.param_types}, used = {sym.used}"
                if isinstance(sym, TypeSymbol):
                    line += f", type={sym.typ}"
                print(line)
