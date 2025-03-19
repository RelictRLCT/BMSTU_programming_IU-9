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

    def isnewline(self) -> bool:
        if self.index == len(self.text):
            return True
        if self.text[self.index] == "\r" and self.index + 1 < len(self.text):
            return self.text[self.index + 1] == "\n"
        return self.text[self.index] == "\n"

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
