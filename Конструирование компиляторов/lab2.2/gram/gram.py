from AST.AST import (Type, ExprPrefOp, ExprBin, ExprTern,
                     ExprPostOp, ExprPostOp2, ExprNum, ExprIdent, Func, \
                    IfStmt, ReturnStmt, ForStmt, \
                    WhileStmt, ExprCall, DeclareStmt, AbsType, BaseType,
                     PointerType, ArrayType, Arg)
from parser import parser_edsl as pe


INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)
DOUBLE = pe.Terminal('DOUBLE', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
IDENT = pe.Terminal('IDENT', '[A-Za-z][A-Za-z0-9]*', str)


def make_keyword(image):
    return pe.Terminal(image, image, lambda name: None, priority=10)


KW_IF, KW_RETURN, KW_FOR, KW_WHILE, KW_INT, KW_DOUBLE, KW_ELSE = \
    map(make_keyword, 'if return for while int double else'.split())


NProgram, NStmt, NFuncs, NType, NFunc = \
    map(pe.NonTerminal, 'Program Stmt Funcs Type Func'.split())

NArgs, NArg1, NArg2, NStar, NArr, NIf = \
    map(pe.NonTerminal, 'Args Arg1 Arg2 Star Arr If'.split())

NReturn, NExpr, NDeclare, NFor, NArr2, NArg, NArgEnd, NArgDec = \
    map(pe.NonTerminal, 'Return Expr Declare For Arr2 Arg ArgEnd ArgDec'.split())

NWhile, NAssExpr, NTernExpr, NCmpExpr, NArithmExpr = \
    map(pe.NonTerminal, 'While AssExpr TernExpr CmpExpr ArithmExpr'.split())

(NTerm, NTerm2, NTerm3, NAssOp, NCmpOp,
 NAddOp, NMulOp, NUnPrefOp, NUnPostOp, NDeclareDir) = \
    map(pe.NonTerminal, 'Term Term2 Term3 AssOp CmpOp '
                        'AddOp MulOp UnPrefOp UnPostOp DeclareDir'.split())

(NArgsCall, NStmts, NUnPostOp2, NDeclareGeneral, NDeclares,
 NDeclareFull, NDeclarePoint, NDeclareEnd, NDeclareReverse, NDeclareDir2, NDeclareEnd2) = \
    map(pe.NonTerminal, 'ArgsCall Stmts UnPostOp2 DeclareGeneral Declares DeclareFull '
                        'DeclarePoint DeclareEnd DeclareReverse DeclareDir2 DeclareEnd2'.split())


NProgram |= NFuncs

NFuncs |= lambda: []
NFuncs |= NFuncs, NFunc, lambda fs, f: fs + [f]

NFunc |= (NType, IDENT, '(', NArgs, ')', '{', NStmts, '}',
          lambda tp, name, args, stmts: Func(tp, name, args, stmts))
#NFunc |= NType, IDENT, '(', ')', '{', NStmts, '}', lambda tp, name, stmts: Func(tp, name, [], stmts)

NStmts |= lambda: []
NStmts |= NStmts, NStmt, ';', lambda stmts, stmt: stmts + [stmt]

NStmt |= NIf, lambda stmt: [stmt]
NStmt |= NDeclareGeneral, lambda stmt: [stmt]
NStmt |= NReturn, lambda stmt: [stmt]
NStmt |= NExpr, lambda stmt: [stmt]
NStmt |= NFor, lambda stmt: [stmt]
NStmt |= NWhile, lambda stmt: [stmt]
NStmt |= '{', NStmts, '}', lambda stmt: [stmt]

NIf |= (KW_IF, '(', NExpr, ')', NStmt, KW_ELSE, NStmt,
        lambda expr, stmts, stmts_else: IfStmt(expr, stmts, stmts_else))


# Хитрая функция для проставления типа уже после рекурсивного составления
def replace_none(type_node, replacement):
    if type_node.base is None:
            type_node.base = BaseType(replacement)
    elif isinstance(type_node, ArrayType):
        replace_none(type_node.base, replacement)
    elif isinstance(type_node, PointerType):
        replace_none(type_node.base, replacement)


def reverse_type_structure(t):
    chain = []
    while t is not None:
        chain.append(t)
        t = t.base

    new_type = None
    for node in chain:
        if isinstance(node, ArrayType):
            new_type = ArrayType(base=new_type, size=node.size)
        elif isinstance(node, PointerType):
            new_type = PointerType(base=new_type)
    return new_type


def set_tp(base_tp: Type, dec):
    dec[0][1] = reverse_type_structure(dec[0][1])

    if dec[0][1] is None:
        dec[0][1] = base_tp
    else:
        replace_none(dec[0][1], base_tp)

    return dec


def set_tp_arg(base_tp: Type, arg):
    arg[1] = reverse_type_structure(arg[1])

    if arg[1] is None:
        arg[1] = base_tp
    else:
        replace_none(arg[1], base_tp)

    return arg


def apply_arrs(dec, arrs):
    tp = dec[1]
    for size in arrs:
        tp = ArrayType(tp, size)
    return tp


def apply_stars(dec, stars):
    tp = dec[1]
    for _ in range(stars):
        tp = PointerType(tp)
    return tp


# Чтоб можно было в одну строку
NDeclareGeneral |= NType, NDeclares, lambda tp_base, decs: [
    DeclareStmt(declare[0][1], declare[0][0], declare[1])
    for declare in [set_tp(tp_base, dec) for dec in decs]
]

NDeclares |= NDeclareFull, lambda dec: [dec]
NDeclares |= NDeclares, ',' , NDeclareFull, lambda decs, dec: decs + [dec]

NDeclareFull |= NDeclare, lambda dec: (dec, None)
NDeclareFull |= NDeclare, '=', NExpr, lambda dec, ex: [dec, ex]

NDeclare |= NStar, NDeclareEnd, lambda stars, dec: [dec[0], apply_stars(dec, stars)]

NDeclareEnd |= IDENT, lambda ident: (ident, None)
NDeclareEnd |= '(', NDeclare, ')', lambda dec: dec
NDeclareEnd |= NDeclareEnd, NArr2, lambda dec, arrs: [dec[0], apply_arrs(dec, arrs)]

NReturn |= KW_RETURN, NExpr, lambda expr: ReturnStmt(expr)

NFor |= (KW_FOR, '(', NExpr, ';', NExpr, ';', NExpr, ')', NStmt,
         lambda expr1, expr2, expr3, stmts: ForStmt(expr1, expr2, expr3, stmts))

NWhile |= KW_WHILE, '(', NExpr, ')', NStmt, lambda expr, stmts: WhileStmt(expr, stmts)

NType |= KW_INT, lambda: Type.Integer
NType |= KW_DOUBLE, lambda: Type.Double


def make_arg(tp_base, arg):
    arg = set_tp_arg(tp_base, arg)
    return [Arg(arg[0], arg[1])]


NArgs |= NArg, lambda arg: [arg]
NArgs |= NArgs, ',', NArg, lambda args, arg: args + [arg]

NArg |= NType, NArgDec, lambda tp_base, arg: make_arg(tp_base, arg)

NArgDec |= NStar, NArgEnd, lambda stars, arg: [arg[0], apply_stars(arg, stars)]

NArgEnd |= IDENT, lambda ident: (ident, None)
NArgEnd |= '(', NArg, ')', lambda arg: arg
NArgEnd |= NArgEnd, NArr2, lambda arg, arrs: [arg[0], apply_arrs(arg, arrs)]


NArgsCall |= lambda: []
NArgsCall |= NExpr, lambda expr: [expr]
NArgsCall |= NExpr, ',', NArgsCall, lambda arg, args: args + [arg]

NStar |= lambda: 0
NStar |= '*', NStar, lambda num: num + 1

NArr |= lambda: 0
NArr |= '[]', NArr, lambda num: num + 1

NArr2 |= lambda: []
NArr2 |= '[', INTEGER,']', NArr2, lambda num, nums: nums + [num]


def make_op_lambda(oper):
    return lambda: oper


for op in ('=', '+=', '-=', '*=', '/='):
    NAssOp |= op, make_op_lambda(op)

for op in ('<', '<=', '>', '>=', '!=', '=='):
    NCmpOp |= op, make_op_lambda(op)


NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'

NUnPrefOp |= '++', lambda: '++'
NUnPrefOp |= '--', lambda: '--'
NUnPrefOp |= '*', lambda: '*'

NUnPostOp |= '++', lambda: '++'
NUnPostOp |= '--', lambda: '--'

NUnPostOp2 |= '[', NExpr, ']', lambda expr: [expr]


NExpr |= NAssExpr
NExpr |= NAssExpr, NAssOp, NExpr, ExprBin

NAssExpr |= NTernExpr
NAssExpr |= NTernExpr, '?', NExpr, ':', NAssExpr, ExprTern

NTernExpr |= NCmpExpr
NTernExpr |= NTernExpr, NCmpOp, NCmpExpr, ExprBin

NCmpExpr |= NArithmExpr
NCmpExpr |= NCmpExpr, NAddOp, NArithmExpr, ExprBin

NArithmExpr |= NTerm
NArithmExpr |= NArithmExpr, NMulOp, NTerm, ExprBin

NTerm |= NTerm2
NTerm |= NUnPrefOp, NTerm, ExprPrefOp

NTerm2 |= NTerm3
NTerm2 |= NTerm2, NUnPostOp, ExprPostOp
NTerm2 |= NTerm2, NUnPostOp2, ExprPostOp2

NTerm3 |= IDENT, ExprIdent
NTerm3 |= INTEGER, ExprNum
NTerm3 |= DOUBLE, ExprNum
NTerm3 |= '(', NExpr, ')', lambda expr: [expr]
NTerm3 |= IDENT, '(', NArgsCall, ')', lambda name, args: ExprCall(name, args)
