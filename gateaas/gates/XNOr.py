from .Gate import Gate

class XNOr(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] == state[1])
