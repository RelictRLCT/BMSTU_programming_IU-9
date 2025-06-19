class SemanticError(Exception):
    def message(self):
        pass


class TokenRule(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Обнаружено правило для токена {self.varname}!'


class RepeatRule(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Обнаружен повтор правила для нетерминала {self.varname}!'


class RepeatType(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Повтор символа {self.varname} в секции %types!'


class MethodTypeMismatch(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Не соответствуют типы элементов альтернативы и типы параметров метода {self.varname}!'


class MoreThenOneType(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return (f'\n{self.pos}: У {self.varname} более одного типизированного элемента альтернативы, '
                f'которая не оканчивается вызовом метода!')


class AlternateTypeMismatch(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Разные типы альтернатив {self.varname}!'


class RepeatMethod(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Повтор имени метода {self.varname}!'


class UndefMethod(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Используемый метод {self.varname} не определён!'


class UnusedMethod(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Метод {self.varname} не используется!'

class UndefNonterminal(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Используемый нетерминал {self.varname} не имеет правила!'

class UntypedNonterminal(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'\n{self.pos}: Нетерминал {self.varname} не имеет типа!'
