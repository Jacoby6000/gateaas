class Gate:
    def __init__(self, inputs, compute):
        self.input_state = []
        while len(self.input_state) < inputs:
            self.input_state.append(False)

        self.output_state = False
        self.compute = compute
        self.dirty = True

    def set(self, input_idx, value):
        if self.input_state[input_idx] != value:
            self.input_state[input_idx] = value
            self.dirty = True

    def probe(self):
        return {
            "inputs": self.input_state,
            "output": self.output_state,
        }

    def is_dirty(self):
        return self.is_dirty

    def run(self):
        if self.dirty:
            self.dirty = False
            result = self.compute(self.input_state)
            self.output_state = result

        return self.output_state


class OR(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] or state[1])

class XOR(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] != state[1])

class XNOR(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] == state[1])

class NOT(Gate):
    def __init__(self):
        super().__init__(1, lambda state: not state[0])

class NOR(Gate):
    def __init__(self):
        super().__init__(2, lambda state: not (state[0] or state[1]))

class NOOP(Gate):
    def __init__(self):
        super().__init__(1, lambda state: state[0])

class NAND(Gate):
    def __init__(self):
        super().__init__(2, lambda state: not (state[0] and state[1]))

class AND(Gate):
    def __init__(self):
        super().__init__(2, lambda state: state[0] and state[1])
