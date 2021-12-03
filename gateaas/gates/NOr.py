from .Gate import Gate

class NOr(Gate):
    def __init__(self):
        super().__init__(2, lambda state: not (state[0] or state[1]))
