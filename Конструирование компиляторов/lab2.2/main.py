from parser import parser_edsl as pe
from pprint import pprint
from gram.gram import NProgram


def main(filename: str) -> None:
    p = pe.Parser(NProgram)

    p.add_skipped_domain('\\s')
    p.add_skipped_domain('//.*?$|/\\*.*?\\*/')

    try:
        with open(filename) as f:
            text = f.read()
            tree = p.parse(text)
            pprint(tree)
    except pe.ParseError as e:
        print(f'Ошибка {e.pos}: {e.message}')
    #except Exception as e:
    #    print(e)


if __name__ == '__main__':
    main('text.txt')
