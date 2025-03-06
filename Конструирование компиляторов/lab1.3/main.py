from models.domain_tags import DomainTag
from models.token import IdentToken, IntToken, IntDoubleToken, DirectToken


def main():
    from models.сompiler import Compiler

    with open("text.txt", "r") as f:
        text = f.read()
    compiler = Compiler()
    scanner = compiler.get_scanner(text)

    token = scanner.next_token()
    while DomainTag(token.tag).value != DomainTag.EOF.value:
        if type(token) == IdentToken:
            print(
                f"{DomainTag(token.tag).name} {token.coords.tostring()}: {compiler.get_name(token.code)}"
            )
        elif type(token) == IntToken or type(token) == IntDoubleToken:
            print(f"{DomainTag(token.tag).name} {token.coords.tostring()}: {token.num}")
        elif type(token) == DirectToken:
            print(
                f"{DomainTag(token.tag).name} {token.coords.tostring()}: {token.value}"
            )
        else:
            print(f"{DomainTag(token.tag).name} {token.coords.tostring()}")
        token = scanner.next_token()
    print(f"{DomainTag(token.tag).name} {token.coords.tostring()}\n")

    print(compiler.output_messages())


if __name__ == "__main__":
    main()
