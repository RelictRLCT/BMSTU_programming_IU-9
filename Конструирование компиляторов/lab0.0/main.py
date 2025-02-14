# Вариант:
# Грамматика:
# <Program>  ::= <Articles> <Body> .
# <Articles> ::= <Article> <Articles> | .
# <Article>  ::= define word <Body> end .
# <Body>     ::= if <Body> <ElsePart> endif <Body>
#              | integer <Body> | word <Body> | .
# <ElsePart> ::= else <Body> | .


def parse(program_string: str) -> list | None:
    raw_tokens = program_string.strip().split()

    tokens = []
    for t in raw_tokens:
        try:
            number = int(t)
            tokens.append(number)
        except ValueError:
            tokens.append(t)

    index = 0
    n = len(tokens)

    def peek_token() -> str | int | None:
        nonlocal index
        if index < n:
            return tokens[index]
        return None

    def get_token() -> str | int | None:
        nonlocal index
        if index < n:
            t = tokens[index]
            index += 1
            return t
        return None

    def expect_token(expected: str) -> bool:
        nonlocal index
        if index < n and tokens[index] == expected:
            index += 1
            return True
        return False

    # <Articles> ::= <Article> <Articles> | .
    def parse_articles() -> dict | None:
        articles_dict = {}
        while True:
            tk = peek_token()
            if tk == 'define':
                article = parse_article()
                if article is None:
                    return None
                name, body_of_article = article
                articles_dict[name] = body_of_article
            else:
                break
        return articles_dict

    # <Article> ::= define word <Body> end
    def parse_article() -> tuple[str, list] | None:
        if not expect_token('define'):
            return None

        name_token = get_token()
        if not isinstance(name_token, str):
            return None

        body = parse_body(end_tokens=['end'])
        if body is None:
            return None

        if not expect_token('end'):
            return None

        return name_token, body

    # <Body> ::= if <Body> <ElsePart> endif <Body> | integer <Body> | word <Body> | .
    def parse_body(end_tokens: list) -> list | None:
        result = []
        while True:
            tk = peek_token()
            if tk is None or tk in end_tokens:
                return result

            if tk == 'if':
                parsed_if = parse_if()
                if parsed_if is None:
                    return None
                result.append(parsed_if)

            else:
                t = get_token()
                if t is None:
                    return result
                if isinstance(t, int):
                    result.append(t)
                elif isinstance(t, str):
                    result.append(t)
                else:
                    return None

    # if <Body> <ElsePart> endif <Body>
    def parse_if() -> list | None:
        if not expect_token('if'):
            return None

        body_if = parse_body(end_tokens=['else', 'endif'])
        if body_if is None:
            return None

        tk = peek_token()
        if tk == 'else':
            get_token()
            body_else = parse_body(end_tokens=['endif'])
            if body_else is None:
                return None
            if not expect_token('endif'):
                return None
            return ["if", body_if, body_else]
        else:
            if not expect_token('endif'):
                return None
            return ["if", body_if]

    # <Program> ::= <Articles> <Body>
    def parse_program():
        articles = parse_articles()
        if articles is None:
            return None

        body = parse_body(end_tokens=[])
        if body is None:
            return None

        if peek_token() is not None:
            return None
        return [articles, body]

    return parse_program()


if __name__ == "__main__":
    test2 = "define abs dup 0 < if -1 * endif end +10 abs -10 abs"
    test = """define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if
                  drop 1
              else =1? if
                  drop 1
              else
                  dup --
                  factorial
                  *
              endif
              endif
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial"""
    test3 = "x dup 0 swap if drop -1 else swap 1 + endif"
    print(parse(test3))
