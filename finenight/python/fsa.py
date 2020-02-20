import copy
import pdb

from error import *
from nameGenerator import PlainIndexNameGenerator
from pprint import pprint, pformat
from state import State



def createStateName(fsa):
    """This function will create a state name that will not conflict
    with a particular fsa
    """
    names = list(fsa.states.keys())
    newName = "newState0"
    index = 1
    if newName in names:
        newName = newName[:-len(str(index))] + str(index)
        index += 1

    return newName


def __rename__(fsa, nameGenerator):

    newKeys = {}
    keysToRename = []
    keysToRename.append(fsa.startState)
    while keysToRename != []:
        currentStateName = keysToRename.pop()
        newKeys[currentStateName] = nameGenerator.generate()
        symbols = list(fsa.states[currentStateName].transitions.keys())
        symbols.sort()
        for symbol in symbols:
            for stateName in fsa.states[currentStateName].transitions[symbol]:
                if stateName not in newKeys and stateName not in keysToRename:
                    keysToRename.append(stateName)
        for stateName in fsa.states[currentStateName].epsilon:
            if stateName not in newKeys and stateName not in keysToRename:
                   keysToRename.append(stateName)

        if keysToRename == []:
            keysToRename = [s for s in list(fsa.states.keys()) if s not in newKeys]
    
    #newKeys = {}
    #for key in fsa.states.keys():
    #    newKeys[key] = nameGenerator.generate()

    return newKeys




def rename(nfa, nameGenerator = None):
    """This function will return a FSA with all states renamed
    according to the nameGenerator class. If the nameGenerator
    is not given, the function will use the PlainIndexNameGenerator
    class.
    """
    
    if nameGenerator is None:
        nameGenerator = PlainIndexNameGenerator()

    newNfa = copy.deepcopy(nfa)
    newKeys = __rename__(nfa, nameGenerator)
    for key in list(nfa.states.keys()):
        newNfa.states[key].name = newKeys[key]
    newNfa.states = dict([(newKeys[s],
                                        newNfa.states[s]) for s in list(newNfa.states.keys())])

    for state in list(newNfa.states.values()):
        state.epsilon = [newKeys[s] for s in state.epsilon]
        for key in list(state.transitions.keys()):
            state.transitions[key] = [newKeys[s] for s in state.transitions[key]]

    #remaping start state and final states
    newNfa.startState = newKeys[nfa.startState]
    newNfa.finalStates = [newKeys[s] for s in nfa.finalStates]

    return newNfa




def binaryOperationRenaming(lhs, rhs, renameStates, nameGenerator):
    """This function will return 2 FSAs with states renamed according
    to the nameGenerator class if the renameStates variable is True,
    otherwise it will check that the two FSAs states names doesn't
    collides. If they don't collide, it will return a unmodified copy
    of the two FSAs instances, if they do, it will raise and exception.
    """
    #this is the renaming procedures
    if renameStates is True:
        if nameGenerator is None:
            nameGenerator = PlainIndexNameGenerator()
        newLhs = rename(lhs, nameGenerator)
        newRhs = rename(rhs, nameGenerator)
    elif isNameColliding(lhs, rhs) is True:
        raise ConstructionError("States names collides when trying " + \
                                "to apply the operation to the FSAs without renaming " + \
                                "states names")
    else:
        newLhs, newRhs = copy.deepcopy(lhs), copy.deepcopy(rhs)

    return (newLhs, newRhs)



class Nfa:
    def __eq__(lhs, rhs):
        okay = True

        if lhs.alphabet != rhs.alphabet:
            okay = False
        elif lhs.startState != rhs.startState:
            okay = False
        elif lhs.finalStates != rhs.finalStates:
            okay = False
        elif list(lhs.states.keys()) == list(rhs.states.keys()):
            for key in list(lhs.states.keys()):
                if not lhs.states[key] == rhs.states[key]:
                    okay = False
        else:
            okay = False

        return okay


    def __ne__(lhs, rhs):
        return not lhs == rhs


    def graphVizExport(self, filename, size = (8,11)):
        """
        This function will export a graphviz file
        """
        f = open(filename, 'w')
        f.write("digraph G {\nrankdir=LR;")
        f.write("size = \"" + str(size[0]) + "," + str(size[1]) + "\";")
	#f.write("rotate = 90;")

        if self.finalStates:
            f.write("node [shape = doublecircle];\n")
            for state in self.finalStates:
                f.write("\"" + str(state) + "\" ")
            f.write(";\n")

        if self.states:
            f.write("node [shape = circle];\n")

##            if self.startState:
##                f.write(" -> " +
##                        self.startState +
##                        ";")
            
            for state in list(self.states.values()):
                for epsilon in state.epsilon:
                    f.write("\"" + str(state.name) +
                            "\" -> \"" + 
                            epsilon +
                            "\" [fontname=\"Symbol\", label=\"e\"];\n")
                for symbol in list(state.transitions.keys()):
                    transitions = state.transitions[symbol]
                    if transitions.__class__ is not [].__class__:
                        transitions = [transitions]
                    for transition in transitions:
                        f.write("\"" + str(state.name) +
                                "\" -> \"" +
                                str(transition) +
                                "\" [ label = \"" +
                                str(symbol) +
                                "\" ];\n")
            f.write("}\n")
            f.close()


    def areEquivalents(self, lhs, rhs):
        okay = True

        lhsIsFinal = False
        rhsIsFinal = False
        if lhs in self.finalStates:
            lhsIsFinal = True
        if rhs in self.finalStates:
            rhsIsFinal = True
        if rhsIsFinal != lhsIsFinal:
            okay = False
        
        if self.states[lhs].transitions != self.states[rhs].transitions:
            okay = False

        if self.states[lhs].epsilon != self.states[rhs].epsilon:
            okay = False

        return okay
            
    
    def __init__(self, states, alphabet, startState, finalStates):
        """
        states is a list of states for this Nfa. (list of State instances)
        alphabet is the alphabet for this Nfa. (list of string)
        startState is the first state (the state instance or the state name)
        finalStates is the list of final states (states instance of the states name)
        """
        self.alphabet = copy.copy(alphabet)

        self.states = dict([(copy.copy(s.name),
                                          copy.deepcopy(s)) for s in states])

        if startState.__class__ != "".__class__:
            startState = startState.name
            
        self.startState = startState

        if finalStates.__class__ != [].__class__:
            finalStates = [finalStates]

        self.finalStates = copy.copy(finalStates)
        for i in range(len(self.finalStates)):
            if self.finalStates[i].__class__ != "".__class__:
                self.finalStates[i] = self.finalStates[i].name


        if startState not in list(self.states.keys()):
            raise ConstructionError("The start state in not part of given states")

        #getting sure that finalStates are part of states
        missingStates = [s for s in self.finalStates if s not in list(self.states.keys())]
        if len(missingStates) > 0:
            output = ""
            for state in missingStates:
                output += state + ", "
            raise ConstructionError( output + "is/are not part of the states")

        #adding missings symbols from the alphabet to states.
        for state in list(self.states.values()):
            #verifying that transitions states are valid
            for key in list(state.transitions.keys()):
                invalidStates = [s for s in state.transitions[key] if s not in list(self.states.keys())]
                if invalidStates != []:
                    raise StateError("state " + \
                                     str(state.name) + \
                                     " has unknown states: " + \
                                     str(invalidStates))
            
            missingInputs = [i for i in self.alphabet if i not in list(state.transitions.keys())]
            for input in missingInputs:
                state.transitions[input] = []

            #verifying that each states has valids symbol according to the alphabet
            invalidInputs = [i for i in list(state.transitions.keys()) if i not in self.alphabet]
            if len(invalidInputs) > 0:
                output = ""
                for input in invalidInputs:
                    output += input + ", "
                raise ConstructionError("symbol " + output[:-2] + "in " + \
                                        str(state.name) + \
                                        " state is/are not part of the alphabet")



    def rename(self):
        return rename(self)



    def isEquivalent(lhs, rhs):
        lhs = lhs.minimize()
        rhs = rhs.minimize()
        return lhs.__eq__(rhs)
    


    def createFrom(self, states = None, alphabet = None, startState = None, finalStates = None):
        if states is None:
            states = list(self.states.values())

        if alphabet is None:
            alphabet = self.alphabet

        if startState is None:
            startState = self.startState

        if finalStates is None:
            finalStates = self.finalStates

        return Nfa(states, alphabet, startState, finalStates)



    def transition(self, states, input):
        if input not in self.alphabet:
            raise AlphabetError(input + " is not in the alphabet")

        #getting all possible epsilons.
        states = self.eClose(states)

        #states will contain all accessible states from the current state,
        #accepting the curent input.
        states = [s for s in states if s.transitions[input]]

        acceptedInputStates = []

        for state in states:
            acceptedInputStates += [self.states[s] for s in self.states[state.name].transitions[input]]

        #e-closing all validated states by the input
        states = self.eClose(acceptedInputStates)

        tmp = []
        for state in states:
            if state not in tmp:
                tmp.append(state)
                
        return tmp


    def transitionMu(self, states, string):
        for index in range(len(string)):
            states = self.transition(states, string[index])

        return states



    def recognize(self, tape):
        """This function returns True if the entire string (tape argument) it is
        pointing at is in the language defined by the FSA, and False if the
        string is not in the language.
        """

        acceptState = False
        states = self.transitionMu([self.states[self.startState]], tape)
        if [s for s in states if s.name in self.finalStates] != []:
            acceptState = True
                
        return acceptState


    def eClose(self, states):
        
        eClosedStates = []

        while(states != []):
            #those states doesn't have any epsilon
            emptyStates = [s for s in states if not s.epsilon]

            states = [s for s in states if s not in emptyStates]
            eClosedStates += emptyStates

            #getting states that have at most 1 epsilon to an another state
            #states = filter(lambda s: s.epsilon, states)

            epsilonStates = []
            for state in states:
                epsilonStates += [s for s in [self.states[s] for s in state.epsilon] if s not in eClosedStates and s not in states]
            eClosedStates += states
            states = epsilonStates

        return eClosedStates

        
    def __str__(self):
        output = ""
        statesName = list(self.states.keys())
        statesName.sort()
        for stateName in statesName:
            state = self.states[stateName]
            output += str(state)
            if stateName == self.startState:
                output += " start!"
            if stateName in self.finalStates:
                output += " final!"
            output += "\n"
        return output



    def isNameColliding(self, otherNfa):
        """This function will return true if there's a name collision
        for states, False otherwise.
        """
        isColliding = False
        if [s in list(lhs.states.keys()) for s in list(rhs.states.keys())]:
            isColliding = True

        return isColliding




    

    # this could overload the "+" operator
    def concatenate(self, other, renameStates = True, nameGenerator = None):
        """this function takes two FSAs and return a
        third FSA that is the concatenation of the firsts.
        """

        lhs, rhs = binaryOperationRenaming(self, other, renameStates, nameGenerator)
            
        #appending rhs startState to all lhs.finalStates
        list(map(lambda s: lhs.states[s].epsilon.append(rhs.startState), lhs.finalStates))
        
        new = Nfa(list(lhs.states.values()) + list(rhs.states.values()),
                  lhs.alphabet + [s for s in rhs.alphabet if s not in lhs.alphabet],
                  lhs.startState,
                  rhs.finalStates)

        return new



    def union(self, other, renameStates = True, nameGenerator = None):
        """this function takes two FSAs and return a
        third FSA that is the union of the firsts.
        """

        lhs = self.determinize()
        rhs = other.determinize()

        newAlphabet = lhs.alphabet + [s for s in rhs.alphabet if s not in lhs.alphabet];

        lhs = Dfa(list(lhs.states.values()),
                  newAlphabet,
                  lhs.startState,
                  lhs.finalStates)
        rhs = Dfa(list(rhs.states.values()),
                  newAlphabet,
                  rhs.startState,
                  rhs.finalStates)

        lhs, rhs = binaryOperationRenaming(lhs, rhs, renameStates, nameGenerator)

        
        lhs.states[lhs.startState] = copy.deepcopy(lhs.states[lhs.startState])
        lhs.states[lhs.startState].epsilon.append(rhs.startState)


        lhs = lhs.determinize()

        new = Nfa(list(lhs.states.values()) + list(rhs.states.values()),
                  lhs.alphabet + [s for s in rhs.alphabet if s not in lhs.alphabet],
                  lhs.startState,
                  rhs.finalStates + lhs.finalStates)
        
        return new



    def plus(self):
        """This function return the + operation over itself
        """

        #only copying states that will change
        newStates = [copy.deepcopy(self.states[s]) for s in [s for s in self.finalStates if self.startState not in self.states[s].epsilon]]

        #those states will be added in the new FSA without being modified
        untouchedStates = [s for s in list(self.states.values()) if s not in newStates]
        
        #appending epsilon from final states to start start.
        list(map(lambda s: s.epsilon.append(self.startState), newStates))
                     
        newStates += untouchedStates
        

        newStates = dict([(s.name, s) for s in newStates])

        return Nfa(list(newStates.values()),
                   self.alphabet,
                   self.startState,
                   self.finalStates)

        


    def kleenee(self):
        """This function return the kleenee of this FSA
        """

        
        newNfa = self.plus()

        #setting the start state as accepting
        if newNfa.startState not in newNfa.finalStates:
            newNfa.finalStates.append(newNfa.startState)

        return newNfa



    def reverse(self):
        """This function return the reversal of a FSA.

        The reversal of a string a(1)a(2)a(3)...a(n) is the string written backwards, that is,
        a(n)a(n-1)a(n-2)...a(1).
        """

        newDfa = self.determinize()

        states = {}
        for state in list(newDfa.states.values()):
            for symbol in list(state.transitions.keys()):
                stateTo = state.transitions[symbol][0]
                if stateTo not in list(states.keys()):
                    states[stateTo] = {}
                if symbol not in states[stateTo]:
                    states[stateTo][symbol] = []
                states[stateTo][symbol].append(state.name)

        states[newDfa.startState] = {}
        states = dict([(k, State(k, states[k])) for k in list(states.keys())])

        startStateName = createStateName(newDfa)
        startState = State(startStateName, epsilon = copy.copy(newDfa.finalStates))
        states[startStateName] = startState

        return Nfa(list(states.values()), newDfa.alphabet, startStateName, [newDfa.startState])



    def complement(self):
        """This function return the complement of the FSA, that is,
        the FSA that recognize all string over Sigma* not recognized by the
        current FSA.

        Note that it will not recognize ALL string not recognized by the current
        FSA but all string, made by the current alphabet, not recognized by the
        current FSA.
        """

        newDfa = self.determinize()
        newDfa.finalStates = [s for s in list(newDfa.states.keys()) if s not in newDfa.finalStates]

        return newDfa


    def intersection(self, other):
        """This function return the intersection of the FSA, that is,
        the FSA that recognize all string over L1 that are recognized by
        the L2.
        """
        
        lhs = self.determinize()
        rhs = other.determinize()

        newAlphabet = lhs.alphabet + [s for s in rhs.alphabet if s not in lhs.alphabet]
        lhs = lhs.createFrom(alphabet = newAlphabet)
        rhs = rhs.createFrom(alphabet = newAlphabet)
        
        lhs = lhs.complement()
        rhs = rhs.complement()
        dfa = lhs.union(rhs)
        dfa = dfa.complement()
        
        return dfa
    

    def difference(self, other):
        """This function returns the difference of the current FSA instance
        and the other FSA, that is, the FSA that accept strings that are in
        language of the current FSA, but not in language of the other FSA.
        """

        lhs = self
        rhs = other.determinize()
        rhs = rhs.createFrom(alphabet = rhs.alphabet + \
                             [s for s in lhs.alphabet if s not in rhs.alphabet])
        rhs = rhs.complement()
        return lhs.intersection(rhs)


    def minimize(self):
        """This function will return the minimized FSA for this FSA, that is,
        the FSA equivalent to this FSA but that the minimum number of states.

        Note that this algorithm is from Brzozowski's method.
        """
        fsa = self

        fsa = fsa.reverse()
        fsa = rename(fsa)
        fsa = fsa.determinize()
        fsa = fsa.reverse()
        fsa = fsa.determinize()
        fsa = rename(fsa)

        return fsa
        


    def determinize(self, rename = True):
        """This function will return the DFA representation of a FSA, that is,
        of a NFA or a DFA.

        Note that if this instance is allready a DFA, it will simply return itself.
        """
        statesDict = {}
        finalStatesDict = {}

        statesToDiscover = [[self.startState]]
        while statesToDiscover != []:
            currentStatesNames = statesToDiscover.pop()
            currentStates = [self.states[s] for s in currentStatesNames]
            statesForAlphabet = dict([(s, self.transition(currentStates, s)) for s in self.alphabet])

            #taking our place
            stateDict = {}
            for symbol in self.alphabet:
                states = [s.name for s in statesForAlphabet[symbol]]
                states.sort()
                stateDict[symbol] = str(states)
            statesDict[str(currentStatesNames)] = State(str(currentStatesNames), stateDict)
            finalStatesDict[str(currentStatesNames)] = [s for s in currentStatesNames if s in self.finalStates]

            for transitions in list(statesForAlphabet.values()):
                transitionsName = [s.name for s in transitions]
                transitionsName.sort()
                if str(transitionsName) not in list(statesDict.keys()):
                     statesToDiscover.append(transitionsName)

        finalStates = [s for s in finalStatesDict if finalStatesDict[s]]

        dfa = Dfa(list(statesDict.values()),
                  self.alphabet,
                  str([self.startState]),
                  finalStates)
        
        return dfa

            


def powerSet(set):
    """This function creates the power set of a set.

    Note: we need to clarify the algorithm.
    """

    myPowerSet = []

    for nbElem in range(len(set) + 1):
        mySubSet = copy.copy(myPowerSet)
        for subset in myPowerSet:
            for element in set:
                tempSet = copy.copy(subset)
                if element not in tempSet:
                    tempSet.append(element)
                tempSet.sort()
                if tempSet not in mySubSet:
                    mySubSet.append(tempSet)
        if [] not in mySubSet:
            mySubSet.append([])
        myPowerSet = copy.copy(mySubSet)
        
    return myPowerSet





        



class Dfa(Nfa):
    """
    This is the Deterministic Finite State Automaton (DFA) class.

    Note that the DFA, by definition, can't have a missing transition
    for a particular symbol in any states. So, if there's some states,
    that are missing some transition for a particular symbol,
    we'll create a supplementary non-accepting state and we will set
    this state for all missing transitions for a symbol. This state
    will have an arc on itself for all symbols over the alphabet of
    this DFA.
    """

    def __init__(self, *args, **kargs):
        #we first initialize this DFA as a NFA
        Nfa.__init__(self, *args, **kargs)

        #checking for the presence of epsilons
        if [s for s in self.states if self.states[s].epsilon] != []:
            raise ConstructionError("You cannot have epsilons in a DFA")

        #checking for the presence of many possible states for the same symbol
        for state in list(self.states.values()):
            for targetStates in list(state.transitions.values()):
                if len(targetStates) > 1:
                    raise StateError("You cannot have 2 possible states " + \
                                            "for the same symbol in a DFA")

        self.__addStateForMissingTransitions__()



    def __addStateForMissingTransitions__(self):
        missingTransitionsStateName = createStateName(self)

        #checking for missing transitions
        for stateName in list(self.states.keys()):
            for symbol in list(self.states[stateName].transitions.keys()):
                if self.states[stateName].transitions[symbol] == []:
                    if missingTransitionsStateName not in list(self.states.keys()):
                        #creating the "missing transitions" state.
                        newState = State(missingTransitionsStateName,
                                         dict([(s, missingTransitionsStateName) for s in self.alphabet]))
                        self.states[missingTransitionsStateName] = newState
                        
                    newState = copy.deepcopy(self.states[stateName])
                    newState.transitions[symbol].append(missingTransitionsStateName)
                    self.states[newState.name] = newState
                    

    def rename(self):
        return rename(self)




    def determinize(self):
        return self







    
