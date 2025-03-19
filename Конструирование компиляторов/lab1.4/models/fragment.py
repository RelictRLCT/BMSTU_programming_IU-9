from models.position import Position


class Fragment:
    def __init__(self, starting: Position, following: Position):
        self.starting = starting
        self.following = following

    def tostring(self) -> str:
        return f"{self.starting.get_coords()}-{self.following.get_coords()}"
