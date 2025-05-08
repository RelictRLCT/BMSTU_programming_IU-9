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
        if type(token) == IdentToken:
            print(
                f"{DomainTag(token.tag).name} {token.coords.tostring()}: {compiler.get_name(token.code)}"
            )
        else:
            print(f"{DomainTag(token.tag).name} {token.coords.tostring()}")
        token = scanner.next_token()
        tokens.append(token)
    print(f"{DomainTag(token.tag).name} {token.coords.tostring()}\n")

    compiler.output_messages()
    print()

    prog = rec_down(tokens, compiler)

    print(prog) # в одну строку
    print()
    print_tree(prog, 0, True)


if __name__ == "__main__":
    main()