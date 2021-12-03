from .Gate import Gate

class Or(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] or state[1])
