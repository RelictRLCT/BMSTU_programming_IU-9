from table_generator.grammar_generator.grammar import Grammar


def print_grammar(g: Grammar):
    print()
    print(f'Терминалы: {g.terms_set}')
    print(f'Нетерминалы: {g.nterms_set}')
    print(f'Аксиома: {g.axiom}')
    for nt, prods in g.rules.items():
        for rhs in prods:
            print(f'{nt[0]:3} -> {" ".join(rhs)}')