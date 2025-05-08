from typing import Any
import dataclasses


def print_tree(node: Any, level: int = 0, is_root: bool = False) -> None:
    pad = '\t' * level

    if is_root:
        print("Prog:")

        for field in dataclasses.fields(node):
            value = getattr(node, field.name)
            print(f"{pad}\t{field.name}:")
            print_tree(value, level + 1)

        return

    if dataclasses.is_dataclass(node):
        for field in dataclasses.fields(node):
            value = getattr(node, field.name)
            print(f"{pad}\t{field.name}:")
            print_tree(value, level + 1)

    elif isinstance(node, list):
        for item in node:
            print_tree(item, level)

    elif isinstance(node, dict):
        for key, value in node.items():
            print(f"{pad}\t{key}:")
            print_tree(value, level + 1)

    else:
        print(f"{pad}\t{node}")