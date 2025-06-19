from lex_analyzer.fragment import Fragment
from table_generator.grammar_generator.grammar import Grammar


def compute_first_sets(g: Grammar):
    first = {t: {t} for t in g.terms_set}
    first['ε'] = {'ε'}
    for nt in g.rules:
        first.setdefault(nt[0], set())

    changed = True
    while changed:
        changed = False
        for nt, prods in g.rules.items():
            for pr in prods:
                before = 0
                print(pr)
                for X in pr:
                    before = len(first[nt[0]])

                    for sym in first[X]:
                        if sym != 'ε' and sym not in first[nt[0]]:
                            first[nt[0]].add(sym)
                            changed = True

                    if 'ε' not in first[X]:
                        break
                else:
                    first[nt[0]].add('ε')
                if len(first[nt[0]]) != before:
                    changed = True
    return first


def compute_follow_sets(g: Grammar, first: dict[str, set[str]]):
    follow = {nt: set() for nt in g.nterms_set}
    follow[g.axiom[0]].add('EOF')

    changed = True
    while changed:
        changed = False
        for A, prods in g.rules.items():
            for pr in prods:
                trailer = follow[A[0]].copy()

                for X in reversed(pr):
                    if X in g.nterms_set:
                        before = len(follow[X])

                        for sym in trailer:
                            if sym not in follow[X]:
                                follow[X].add(sym)
                                changed = True

                        if 'ε' in first[X]:
                            for sym in first[X]:
                                if sym != 'ε' and sym not in trailer:
                                    trailer.add(sym)
                        else:
                            trailer = first[X].copy()
                    else:
                        trailer = first[X].copy()
    return follow
