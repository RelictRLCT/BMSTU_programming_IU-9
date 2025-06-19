nterms = ['S', 'AxDecl', 'Rules', 'Rule', "Rules'", 'Prods', 'Prod', "Prods'", 'Expr', 'Symbol']
terms = ['TERM', 'NTERM', 'L_Q_PAREN', 'R_Q_PAREN', 'AXIOM', 'EOF']

predict_table = {
    'S': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['AxDecl', 'Rules'],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'AxDecl': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['L_Q_PAREN', 'AXIOM', 'L_Q_PAREN', 'NTERM', 'R_Q_PAREN', 'R_Q_PAREN'],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'Rules': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['Rule', "Rules'"],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    "Rules'": {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['Rule', "Rules'"],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ε']
    },
    'Rule': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['L_Q_PAREN', 'NTERM', 'Prods', 'R_Q_PAREN'],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'Prods': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['Prod', "Prods'"],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    "Prods'": {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['Prod', "Prods'"],
        'R_Q_PAREN': ['ε'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'Prod': {
        'TERM': ['ERROR'],
        'NTERM': ['ERROR'],
        'L_Q_PAREN': ['L_Q_PAREN', 'Expr', 'R_Q_PAREN'],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'Expr': {
        'TERM': ['Symbol', 'Expr'],
        'NTERM': ['Symbol', 'Expr'],
        'L_Q_PAREN': ['ERROR'],
        'R_Q_PAREN': ['ε'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
    'Symbol': {
        'TERM': ['TERM'],
        'NTERM': ['NTERM'],
        'L_Q_PAREN': ['ERROR'],
        'R_Q_PAREN': ['ERROR'],
        'AXIOM': ['ERROR'],
        'EOF': ['ERROR']
    },
}
