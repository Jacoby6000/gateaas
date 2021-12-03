from .Gate import Gate

class XOr(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] != state[1])
