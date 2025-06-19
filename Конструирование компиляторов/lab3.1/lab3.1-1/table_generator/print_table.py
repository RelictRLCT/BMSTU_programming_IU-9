from table_generator.grammar_generator.grammar import Grammar


def print_table(table: dict[str, dict[str, list[str]]], gr: Grammar):
    with open('../predict_table/predict_table.py', 'w') as f:
        s = "terms = ["
        for t in gr.terms_set:
            s += f"'{t}', "
        s += ']\n'
        f.write(s)

        s = "nterms = ["
        for nt in gr.nterms_set:
            s += f'"{nt}", '
        s += ']\n'
        f.write(s)

        f.write("predict_table = {\n")
        for nt, row in table.items():
            f.write(f"    {repr(nt)}: {{\n")
            for term, rule in row.items():
                rule_str = "[" + ", ".join(repr(sym) for sym in rule) + "]"
                if term != 'ε':
                    f.write(f"        {repr(term)}: {rule_str},\n")
            f.write("    },\n")
        f.write("}\n")
