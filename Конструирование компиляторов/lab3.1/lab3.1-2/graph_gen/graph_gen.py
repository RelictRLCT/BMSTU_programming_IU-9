from syntax.AST import Inner, Node


def graph_gen(tree: Inner) -> None:
    counter = 0
    lines: list[str] = []

    def visit(node: Node, parent_id: int | None = None):
        nonlocal counter
        node_id = counter
        counter += 1

        if isinstance(node, Inner):
            label = node.rule.replace('"', r'\"')
        else:
            label = f'{node.token.name}: {str(node.token.value).replace('"', r'\"')}'

        lines.append(f'  node{node_id} [label="{label}"];')

        if parent_id is not None:
            lines.append(f'  node{parent_id} -> node{node_id};')

        if isinstance(node, Inner):
            for child in node.children:
                visit(child, node_id)

    visit(tree)

    with open('graph.dot', 'w') as f:
        f.write('digraph {\n')
        for ln in lines:
            f.write(ln + '\n')
        f.write('}\n')
