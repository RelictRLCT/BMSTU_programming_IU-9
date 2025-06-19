import graphviz


def visualize_graph(inpath: str, outpath: str) -> None:
    with open(inpath, 'r', encoding='utf-8') as f:
        src = f.read()

    dot = graphviz.Source(src)
    dot.render(filename=outpath, format='png')