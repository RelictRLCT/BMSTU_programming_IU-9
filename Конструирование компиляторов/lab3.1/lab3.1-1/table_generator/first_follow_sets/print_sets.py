def print_sets(firsts, follows, nterms) -> None:
    print()
    print('Множества FIRST и FOLLOW для нетерминалов')
    for nt in nterms:
        print(f'{nt:3} - FIRST: {firsts[nt]}, FOLLOW: {follows[nt]}')