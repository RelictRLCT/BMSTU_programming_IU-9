from lex_analyzer.domain_tags import DomainTag
from lex_analyzer.token import Token


class Node:
    def print(self, level: int):
        pass


class Leaf(Node):
    def __init__(self, token: Token):
        self.token = token

    def print(self, level: int):
        print(f'{"\t" * level}{DomainTag(self.token.tag).name}: {self.token.value}')


class Inner(Node):
    def __init__(self, rule: str, children: list[Node]):
        self.rule = rule
        self.children = children

    def print(self, level: int):
        print("\t" * level + str(self.rule))
        for child in self.children:
            child.print(level + 1)
