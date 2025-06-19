from calculator.calculator import calculate
from graph_gen.graph_gen import graph_gen
from graph_gen.graph_visualize import visualize_graph
from lex_analyzer.token import Token
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
    while token.name != 'EOF':
        print(
            f"{token.name} {token.coords.tostring()}: {token.value}"
        )
        token = scanner.next_token()
        tokens.append(token)
    print(f"{token.name} {token.coords.tostring()}: {token.value}\n")

    compiler.output_messages()

    res, tree = top_down_parse(tokens)

    graph_gen(tree)
    visualize_graph('graph.dot', 'graph')

    result = calculate(tree)
    print(f'{text} = {result}')


if __name__ == "__main__":
    main()
