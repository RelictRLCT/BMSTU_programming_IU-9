import abc
import enum
from dataclasses import dataclass


# Type → int | double
class Type(enum.Enum):
    Integer = 'int'
    Double = 'double'


class Arg(abc.ABC):
    pass


class AbsType(abc.ABC):
    pass


@dataclass
class Arg(Arg):
    type: AbsType
    name: str


class Stmt(abc.ABC):
    pass


class Expr(abc.ABC):
    pass


# If -> if (Expr) {stmt*} else {stmt*}
@dataclass
class IfStmt(Stmt):
    expr: Expr
    then: list[Stmt]
    else_: list[Stmt]


@dataclass
class BaseType(AbsType):
    base: Type

@dataclass
class PointerType(AbsType):
    base: AbsType

@dataclass
class ArrayType(AbsType):
    base: AbsType
    size: int


# int *x[10] -> ArrayType(PointerType(BaseType(Type.INT)), 10)
# int (*x)[10] -> PointerType(ArrayType(BaseType(Type.INT), 10))


@dataclass
class DeclareStmt(Stmt):
    type: AbsType
    name: str
    expr: Expr | None


# Return -> return Expr
@dataclass
class ReturnStmt(Stmt):
    expr: Expr


# For -> for (stmt; stmt; stmt) {stmt*}
@dataclass
class ForStmt(Stmt):
    expr1: Expr
    expr2: Expr
    expr3: Expr
    stmts: list[Stmt]


# While -> while (Expr) {stmt*}
@dataclass
class WhileStmt(Stmt):
    expr: Expr
    stmts: list[Stmt]


# Expr -> IDENT
@dataclass
class ExprIdent(Expr):
    name: str


# Expr -> NUMBER
@dataclass
class ExprNum(Expr):
    num: int | float


# Expr -> Expr BinOp Expr
# BinOp -> * | / | + | - | = | += | -= | *= | /= | > | >= | < | <= | != | ==
@dataclass
class ExprBin(Expr):
    left: Expr
    op: str
    right: Expr


# Expr -> UnPrefOp Expr
# UnPrefOp -> ++ | -- | *
@dataclass
class ExprPrefOp(Expr):
    op: str
    expr: Expr


# Expr -> Expr UnPostOp
# UnPostOp -> ++ | --
@dataclass
class ExprPostOp(Expr):
    op: str
    expr: Expr


# Expr -> Expr UnPostOpArr
# UnPostOpArr -> [Expr]
@dataclass
class ExprPostOp2(Expr):
    op = '[]'
    expr: Expr
    expr_arr: Expr


# Expr -> Expr ? Expr : Expr
@dataclass
class ExprTern(Expr):
    expr1: Expr
    expr2: Expr
    expr3: Expr


# Expr -> IDENT(args)
@dataclass
class ExprCall(Expr):
    name: str
    args: list[Expr]


# Func -> Type IDENT(arg, ... arg) {stmt*}
@dataclass
class Func:
    type: Type
    name: str
    args: list[Arg]
    stmts: list[Stmt]


# Program → Funcs
@dataclass
class Program:
    funcs : list[Func]
