from .Gate import Gate

class NoOp(Gate):
    def __init__(self):
        super().__init__(1, lambda state: state[0])
