from syntax.AST import Node
from table_generator.first_follow_sets.compute_sets import compute_first_sets, compute_follow_sets
from table_generator.first_follow_sets.print_sets import print_sets
from table_generator.grammar_generator.grammar_gen import build_grammar
from table_generator.grammar_generator.print_grammar import print_grammar
from table_generator.print_table import print_table


def table_gen(tree: Node) -> None:
    gr = build_grammar(tree)
    print_grammar(gr)

    firsts = compute_first_sets(gr)
    follows = compute_follow_sets(gr, firsts)
    print_sets(firsts, follows, gr.nterms_set)

    table: dict[str, dict[str, list[str]]] = {}
    for nt in gr.nterms_set:
        table[nt] = {}
        for t in gr.terms_set:
            table[nt][t] = ['ERROR']

    for nt, prods in gr.rules.items():
        for pr in prods:
            if pr[0] in gr.terms_set:
                if table[nt[0]][pr[0]] == ['ERROR']:
                    table[nt[0]][pr[0]] = pr
                else:
                    print(f"\nERROR: заданная грамматика не LL-1!\nКонфликт в {nt} -> {prods}")
                    exit(1)
            else:
                for t in firsts[pr[0]]:
                    if t != 'ε':
                        if table[nt[0]][t] == ['ERROR']:
                            table[nt[0]][t] = pr
                        else:
                            print(f"\nERROR: заданная грамматика не LL-1!\nКонфликт в {nt} -> {prods}")
                            exit(1)
                if 'ε' in firsts[pr[0]]:
                    for b in follows[nt[0]]:
                        if table[nt[0]][b] == ['ERROR']:
                            table[nt[0]][b] = pr
                        else:
                            print(f"\nERROR: заданная грамматика не LL-1!\nКонфликт в {nt} -> {prods}")
                            exit(1)

    print_table(table, gr)
