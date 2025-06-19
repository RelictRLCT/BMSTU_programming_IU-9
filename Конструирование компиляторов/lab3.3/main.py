from visualize.print_tree import print_tree
from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import Token, IdentToken
from syntax.parser import rec_down


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
        token = scanner.next_token()
        tokens.append(token)

    compiler.output_messages()

    rec_down(tokens, compiler)


if __name__ == "__main__":
    main()
