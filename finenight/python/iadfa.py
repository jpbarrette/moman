from fsa import *
from nameGenerator import *


class IncrementalAdfa(Dfa):
    """This class is an Acyclic Deterministic Finite State Automaton
    constructed by a list of words.
    """

    def __init__(self, words, nameGenerator = None, sorted = False):

        if nameGenerator is None:
            nameGenerator = IndexNameGenerator()
            
        self.nameGenerator = nameGenerator
        if sorted:
            self.createFromSortedListOfWords(words)
        else:
            self.createFromArbitraryListOfWords(words)


        

    def getCommonPrefix(self, word):
        stateName = self.startState

        index = 0
        nextStateName = stateName
        while nextStateName is not None:
            symbol = word[index]
            stateName = nextStateName
            if self.states[stateName].transitions.has_key(symbol):
                nextStateName = self.states[stateName].transitions[symbol]
                index += 1
            else:
                nextStateName = None

        return (stateName, word[index:])




    def hasChildren(self, stateName):
        okay = False
        if filter(lambda s: s, self.states[stateName].transitions.values()):
            okay = True

        return okay




    def addSuffix(self, stateName, currentSuffix):
        lastState = stateName
        while len(currentSuffix) > 0:
            newStateName = self.nameGenerator.generate()
            symbol = currentSuffix[0]
            currentSuffix = currentSuffix[1:]
            self.states[stateName].transitions[symbol] = newStateName
            self.states[newStateName] = State(newStateName)
            stateName = newStateName
        self.finalStates.append(stateName)
        


    def markedAsRegistered(self, stateName):
        return self.register.has_key(stateName)


    def markAsRegistered(self, stateName):
        self.register[stateName] = True





    def equivalentRegisteredState(self, stateName):
        equivatentState = None
        
        for state in self.register.keys():
            if self.areEquivalents(state, stateName):
                equivatentState = state

        return equivatentState
            

    def lastChild(self, stateName):
        input = self.states[stateName].transitions.keys()
        input.sort()
        return (self.states[stateName].transitions[input[-1]], input[-1])
        

    def replaceOrRegister(self, stateName):
        #childName = self.finalStates[-1]
        childName, lastSymbol = self.lastChild(stateName)
        if not self.markedAsRegistered(childName):
            if self.hasChildren(childName):
                self.replaceOrRegister(childName)
            equivalentState = self.equivalentRegisteredState(childName)
            if equivalentState is not None:
                self.deleteBranch(childName)
                self.states[stateName].transitions[lastSymbol] = equivalentState
            else:
                self.markAsRegistered(childName)


    def deleteBranch(self, child):
        childs = [child]
        while len(childs) > 0:
            nextChilds = []
            for child in childs:
                nextChilds += filter(lambda s: not self.markedAsRegistered(s), self.states[child].transitions.values())
                self.states.pop(child)
                if child in self.finalStates:
                    self.finalStates.remove(child)
            childs = nextChilds



    def createFromSortedListOfWords(self, words):
        self.register = {}

        self.finalStates = []
        self.startState = self.nameGenerator.generate()
        self.states = {self.startState : State(self.startState)}

        lastWord = None
        for word in words:
            if word.endswith('\n'):
              word = word[:-1]
            lastStateName, currentSuffix = self.getCommonPrefix(word)
            if self.hasChildren(lastStateName):
                self.replaceOrRegister(lastStateName)
            self.addSuffix(lastStateName, currentSuffix)
        self.replaceOrRegister(self.startState)
    

    def createFromArbitraryListOfWords(self, words):
        self.register = {}

        self.finalStates = []
        self.startState = self.nameGenerator.generate()
        self.states = {self.startState : State(self.startState)}
        
