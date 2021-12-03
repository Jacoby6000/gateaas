from .Gate import Gate

class And(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] and state[1])
