from syntax.AST import Node, Inner


def calculate(tree: Node):
    def visit(node: Node) -> int:
        if isinstance(node, Inner):
            match node.rule.split()[0]:
                case 'E':
                    if len(node.children) == 2:
                        return visit(node.children[0]) + visit(node.children[1])
                    else:
                        return visit(node.children[0])
                case 'T':
                    if len(node.children) == 2:
                        return visit(node.children[0]) * visit(node.children[1])
                    else:
                        return visit(node.children[0])
                case "E'":
                    if len(node.children) == 3:
                        return visit(node.children[1]) + visit(node.children[2])
                    elif len(node.children) == 1:
                        return visit(node.children[0])
                    else:
                        return 0
                case "T'":
                    if len(node.children) == 3:
                        return visit(node.children[1]) * visit(node.children[2])
                    elif len(node.children) == 1:
                        return visit(node.children[0])
                    else:
                        return 1
                case 'F':
                    if len(node.children) == 3:
                        return visit(node.children[1])
                    else:
                        return visit(node.children[0])
        else:
            return node.token.value

    return visit(tree)
