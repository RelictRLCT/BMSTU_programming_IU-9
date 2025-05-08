from graph_gen.graph_gen import graph_gen
from graph_gen.graph_visualize import visualize_graph
from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import TermToken, NTermToken, Token
from syntax.parser import top_down_parse


def main():
    from lex_analyzer.сompiler import Compiler

    with open("text.txt", "r") as f:
        text = f.read()
    compiler = Compiler()
    scanner = compiler.get_scanner(text)

    tokens: list[Token] = []

    token = scanner.next_token()
    tokens.append(token)
    while DomainTag(token.tag) != DomainTag.EOF:
        if type(token) == TermToken or type(token) == NTermToken:
            print(
                f"{DomainTag(token.tag).name} {token.coords.tostring()}: {token.value}"
            )
        else:
            print(f"{DomainTag(token.tag).name} {token.coords.tostring()}")
        token = scanner.next_token()
        tokens.append(token)
    print(f"{DomainTag(token.tag).name} {token.coords.tostring()}\n")

    compiler.output_messages()
    print()

    res, tree = top_down_parse(tokens)
    for rule in res:
        print(rule)

    print()
    tree.print(0)

    graph_gen(tree)
    visualize_graph('graph.dot', 'graph')


if __name__ == "__main__":
    main()
