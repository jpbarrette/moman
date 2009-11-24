from fsa import Nfa

import copy
import pdb

from transition import *
from error import *
from nameGenerator import IndexNameGenerator
from state import State




class FstState(State):
    def __init__(self, name):
        State.__init__(self, name)

    def __str__(self):
        output = ""
        for transitionList in self.transitions.values():
            for transition in transitionList:
                output += str(transition) + "\n"
        return output



    def add(self, transition):
        """
        With this function you can add a 3-tuple, a 4-tuple
        transition or a Transition object.
        """
        if transition.__class__ != Transition:
            transition = Transition(transition)
            
        if self.name is None:
            self.name = transition.start
        elif self.name != transition.start:
            raise ConstructionError( "The transition that you are trying to add is not part of this state")

        if not self.transitions.has_key(transition.input):
            self.transitions[transition.input] = []
            
        self.transitions[transition.input].append(transition)



    def inverse(self):
        other = copy.copy(self)
        for key in other.transitions.keys():
            other.transitions[key] = map(lambda s: s.inverse(), other.transitions[key])

        return other

            
    def __eq__(lhs, rhs):
        okay = True

        if lhs.name != rhs.name:
            okay = False

        if lhs.transitions.keys() != rhs.transitions.keys():
            okay = False
            pdb.set_trace()

        if okay is not False:
            for key in lhs.transitions.keys():
                if lhs.transitions[key] != rhs.transitions[key]:
                    okay = False

        return okay



    def __ne__(lhs, rhs):
        return not lhs.__eq__(rhs)
        
        
        


    

    



class Fst(Nfa):
    """This class represent a Finite-State Transducer
    Each state(FstState) is a list of transitions (4-tuple or a
    Transition)

    (Q, E1*, E2*, Q) , The input state, The input symbol,
    The output symbol and the The output state, respectely.
    """
    def __init__(self, states, alphabet1, alphabet2, startState, finalStates):

        self.alphabet = alphabet1
        self.alphabet2 = alphabet2

        #adding states to the Fst
        self.states = {}
        for state in states:
            self.add(state)

        self.startState = str(startState)

        #we ensure that finalStates is a list
        if finalStates.__class__ != list:
            finalStates = [finalStates]

        #we ensure that each element of finalStates is a string
        self.finalStates = copy.copy(finalStates)
        for i in range(len(self.finalStates)):
            if hasattr(self.finalStates[i], "name"):
                self.finalStates[i] = self.finalStates[i].name
            else:
                self.finalStates[i] = str(self.finalStates[i])
                


    def __str__(self):
        output = "starting state: " + str(self.startState) + "\n"
        output += "final states: " + str(self.finalStates) + "\n"
        for state in self.states.values():
            output += str(state)
            
        return output

        

    def add(self, state):
        """This function gives you the ability to add a transition
        (Transition, or a 4-tuple), or a FstState to this FST.
        """
        if state.__class__ == tuple:
            state = Transition(state)
        
        if state.__class__ == Transition:
            if not self.states.has_key(state.start):
                self.states[state.start] = FstState(state.start)
            self.states[state.start].add(state)

        if state.__class__ == FstState:
            self.states[state.name] = state


    def inverse(self):
        other = copy.copy(self)
        other.states = dict(map(lambda s: (s.name, s.inverse()), other.states.values()))

        return other
    
            

        

        
