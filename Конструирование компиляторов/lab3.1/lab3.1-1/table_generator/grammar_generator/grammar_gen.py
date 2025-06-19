from lex_analyzer.fragment import Fragment
from lex_analyzer.position import Position
from lex_analyzer.token import Token
from syntax.AST import Node, Leaf, Inner
from table_generator.grammar_generator.grammar import Grammar


def get_tag(leaf):
    return leaf.token.name


# Поиск всех листьев поддерева
def collect_leaves(node: Node, out: list[Leaf]) -> None:
    if isinstance(node, Leaf):
        out.append(node)
    elif isinstance(node, Inner):
        for ch in node.children:
            collect_leaves(ch, out)


# Поиск всех узлов Prod
def collect_prod_nodes(node: Inner, out: list[Inner]) -> None:
    head = node.rule.split()[0]
    if head == 'Prod':
        out.append(node)
    for ch in node.children:
        if isinstance(ch, Inner):
            collect_prod_nodes(ch, out)


def build_grammar(tree_root: Node) -> Grammar:
    G = Grammar()

    def scan_expr(expr_node: Inner | None) -> list[str]:
        if expr_node is None:
            return ['ε', (0, 0)]

        symbols: list[str] = []
        leaves: list[Leaf] = []
        collect_leaves(expr_node, leaves)
        for leaf in leaves:
            tag = get_tag(leaf)
            if tag == 't':
                G.terms.add((leaf.token.value, leaf.token.coords))
                G.terms_set.add(leaf.token.value)
                symbols.append(leaf.token.value)
            elif tag == 'nt':
                G.nterms.add((leaf.token.value, leaf.token.coords))
                G.nterms_set.add(leaf.token.value)
                symbols.append(leaf.token.value)
        return symbols or ['ε']

    # обход всего дерева
    def visit(node: Node):
        if not isinstance(node, Inner):
            return

        nt = node.rule.split()[0]

        # AxDecl
        if nt == 'AxDecl':
            # поиск листа с NTERM
            leaves: list[Leaf] = []
            collect_leaves(node, leaves)
            for leaf in leaves:
                if get_tag(leaf) == 'nt':
                    if G.axiom is not None:
                        raise ValueError(f'{leaf.token.coords.tostring()}: больше одной аксиомы')
                    G.axiom = leaf.token.value
                    G.nterms.add((leaf.token.value, leaf.token.coords))
                    G.nterms_set.add(leaf.token.value)
                    break

        # правила
        elif nt == 'Rule':  # [NTERM Prods]
            # NTERM
            lhs = None
            for child in node.children:
                if isinstance(child, Leaf) and get_tag(child) == 'nt':
                    lhs = (child.token.value, child.token.coords)
                    G.nterms.add(lhs)
                    G.nterms_set.add(child.token.value)
                    break
            if lhs is None:
                raise ValueError(f'Правило {node.rule} без NTERM')

            # Prods
            prods_node = None
            for child in node.children:
                if isinstance(child, Inner) and child.rule.split()[0] == 'Prods':
                    prods_node = child
                    break
            if prods_node is None:
                raise ValueError(f'Правило {node.rule} без Prods')

            # все Prod-ы
            prod_nodes: list[Inner] = []
            collect_prod_nodes(prods_node, prod_nodes)

            for prod in prod_nodes:
                # Expr внутри Prod
                expr_node = None
                for c in prod.children:
                    if isinstance(c, Inner) and c.rule.split()[0] == 'Expr':
                        expr_node = c
                        break
                rhs = scan_expr(expr_node)

                if rhs not in G.rules[lhs]:
                    G.rules[lhs].append(rhs)

        # Symbol
        elif nt == 'Symbol':
            for child in node.children:
                if isinstance(child, Leaf):
                    tag = get_tag(child)
                    if tag == 't':
                        G.terms.add((child.token.value, child.token.coords))
                        G.terms_set.add(child.token.value)
                    else:
                        G.nterms.add((child.token.value, child.token.coords))
                        G.nterms_set.add(child.token.value)
                    break

        for ch in node.children:
            visit(ch)

    visit(tree_root)

    if G.axiom is None:
        raise ValueError('аксиома не найдена')

    # каждый нетерминал должен иметь правила
    for nt in G.nterms:
        nts = [rule[0] for rule in G.rules]
        if nt[0] not in nts:
            raise ValueError(f'{nt[1].tostring()}: нетерминал "{nt[0]}" без правил')

    # все символы в правой части правил должны быть объявлены
    for lhs, alts in G.rules.items():
        for rhs in alts:
            for sym in rhs:
                if sym == 'ε':
                    continue
                if sym not in G.nterms_set and sym not in G.terms_set:
                    raise ValueError(
                        f'{lhs[1].tostring()}: символ "{sym}" в правиле "{lhs} -> {rhs}" не объявлен'
                    )

    G.terms.add(('EOF', None))
    G.terms_set.add('EOF')
    return G
