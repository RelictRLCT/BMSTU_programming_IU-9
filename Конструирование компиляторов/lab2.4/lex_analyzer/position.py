import unicodedata


class Position:
    def __init__(self, text: str):
        self.text = text
        self.line = self.pos = 1
        self.index = 0

    def get_coords(self) -> str:
        return f"({self.line}, {self.pos})"

    def cp(self) -> str:
        if self.index == len(self.text):
            return ""
        return self.text[self.index]

    def peek_word(self, num: int):
        if self.index + num > len(self.text):
            return ""
        return self.text[self.index:self.index + num]

    def isnewline(self) -> bool:
        if self.index == len(self.text):
            return True
        if self.text[self.index] == "\r" and self.index + 1 < len(self.text):
            return self.text[self.index + 1] == "\n"
        return self.text[self.index] == "\n"

    def iswhitespace(self) -> bool:
        return self.index != len(self.text) and self.text[self.index].isspace()

    def isletter(self) -> bool:
        return self.index != len(self.text) and self.text[self.index].isalpha()

    def is_letter_digit_or_thief(self) -> bool:
        return self.index != len(self.text) and (
            self.text[self.index].isalnum() or self.text[self.index] == "-"
        )

    def isnumber(self) -> bool:
        return self.index != len(self.text) and self.text[self.index].isnumeric()

    def next(self) -> None:
        if self.index < len(self.text):
            if self.isnewline():
                if self.text[self.index] == "\r":
                    self.index += 1
                self.line += 1
                self.pos = 1
            else:
                self.pos += 1
            self.index += 1
