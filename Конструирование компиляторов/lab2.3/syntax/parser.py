from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import Token
from syntax.AST import Inner, Leaf
from syntax.predict_table import predict_table
from syntax.symbols import terms

stack = []


def top_down_parse(u: list[Token]):
    result = []

    root = Inner("S' -> S EOF", [])

    stack.append(('EOF', root))
    stack.append(('S', root))
    i = 0
    tok = u[i]
    a = DomainTag(tok.tag).name

    while True:
        X, inner = stack.pop()
        if X == 'EOF':
            break
        if X == 'ε':
            continue
        if X in terms:
            if X == a:
                inner.children.append(Leaf(tok))
                i += 1
                tok = u[i]
                a = DomainTag(tok.tag).name
            else:
                print(f'SYNTAX ERROR: {u[i].coords.tostring()}')
                exit(1)
        elif predict_table[X][a] != ['ERROR']:
            new_inner = Inner(f'{X} -> {predict_table[X][a]}', [])
            inner.children.append(new_inner)

            for sym in reversed(predict_table[X][a]):
                stack.append((sym, new_inner))
            result.append(f'{X} -> {predict_table[X][a]}')
        else:
            print(f'SYNTAX ERROR: {u[i].coords.tostring()}')
            exit(1)

    return result, root.children[0]
