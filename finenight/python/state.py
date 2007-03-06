import copy


class DfaState:
    """This class is representing a DFA state.

    Don't use this class directly.
    """

    def __init__(self, state):
        raise NotImplemented()
        self.name = state.name

        if filter(lambda s: len(s) > 1, state.transitions.keys()):
            raise StateError("A DFA transition is containing more than " + \
                             "one state for a symbol")

        self.transitions = dict(map(lambda k: (k, state.transitions[k][0]),
                               state.transitions.keys()))
        


    def __str__(self):
        """
        This offers a string version of a state.
        """
        stringVal = "name: " + self.name + ", transitions:" + str(self.transitions)
        
        return stringVal


    def __eq__(lhs, rhs):
        okay = True

        if lhs.name != rhs.name:
            okay = False

        if lhs.transitions != rhs.transitions:
            okay = False

        return okay



    def __ne__(lhs, rhs):
        return not lhs.__eq__(rhs)




class State(DfaState):
    """This class is representing a NFA state.
    """

    def __init__(self, name, transitions = None, epsilon = None):
        """transitions are a map.
        ex: { input1 : nameOfOtherState, input2 : nameOfOtherState2 }
        """
        self.name = name
        if epsilon is None:
            epsilon = []
        self.epsilon = epsilon

        if transitions is None:
            transitions = {}
            
        self.transitions = transitions

        for key in self.transitions.keys():
            if type(self.transitions[key]) != type([]):
                self.transitions = copy.copy(self.transitions)
                self.transitions[key] = [self.transitions[key]]
            



    def toDfaState(self):
        return DfaState(self)





    def __str__(self):
        """
        This offers a string version of a state.
        """
        
        return DfaState.__str__(self) + "  epsilon:" + str(self.epsilon) 




    def __eq__(lhs, rhs):
        okay = DfaState.__eq__(lhs,rhs)

        if lhs.epsilon != rhs.epsilon:
            okay = False

        return okay




    def __ne__(lhs, rhs):
        return not lhs.__eq__(rhs)


