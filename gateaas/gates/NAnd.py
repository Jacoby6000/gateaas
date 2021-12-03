from .Gate import Gate

class NAnd(Gate):
    def __init__(self):
        super().__init__(2, lambda state: not (state[0] and state[1]))
