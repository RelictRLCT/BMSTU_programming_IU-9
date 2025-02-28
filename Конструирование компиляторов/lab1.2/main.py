import re

DOMAINS = [
    ("KEYWORD", re.compile(r'(/while/|/do/|/end/)')),
    ("IDENT", re.compile(r'/([^/]+)/')),
    ("COMMENT", re.compile(r'//.*')),
]


class LexAnalyzer:
    def __init__(self, path: str):
        with open(path, "r") as f:
            self.text = f.read()
        self.i = 0

    def next_token(self) -> tuple[str, str, tuple[int, int]]:
        while self.i < len(self.text):
            if self.text[self.i].isspace():
                self.delete_spaces()

            for name, pattern in DOMAINS:
                m = pattern.match(self.text, pos=self.i)
                if m:
                    self.i = m.end()
                    return name, m.group(0), self.calc_pos(m.start())
            error_pos = self.calc_pos(self.i)
            self.i += 1
            return "", "ERROR", error_pos

    def delete_spaces(self):
        while self.i < len(self.text) and self.text[self.i].isspace():
            self.i += 1

    def calc_pos(self, start: int) -> tuple[int, int]:
        col = st = i = 0
        while i < len(self.text):
            if i == start:
                break
            col += 1
            if self.text[i] == '\n':
                st += 1
                col = 0
            i += 1

        return st+1, col+1

def main():
    lex = LexAnalyzer("text.txt")
    token = lex.next_token()
    tag, attr, coords = token
    print(f'{tag} {coords}: {attr}')
    while token is not None:
        token = lex.next_token()
        try:
            tag, attr, coords = token
            print(f'{tag} {coords}: {attr}')
        except TypeError:
            print(token)


if __name__ == '__main__':
    main()
