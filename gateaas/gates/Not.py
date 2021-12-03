from .Gate import Gate

class Not(Gate):
    def __init__(self):
        super().__init__(1, lambda state: not state[0])
