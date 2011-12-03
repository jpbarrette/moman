import fsc
from copy import copy
from pdb import set_trace
from pprint import pprint

##############################################################
#
# Definitions
#
##############################################################
#
# 1. Relevant Subworld for a state: NEED to be done.
# 
##############################################################


def getNonSubsumingPositionsForBasePosition(n, pos, basePos):
    """
Returns all positions subsumed by base position, but
aren't subsuming the given position.
"""
    positions = []
    j = pos.i + 1
    maxDistance = basePos.i + n + 1
    maxHigh = 0
    while j < maxDistance:
        f = pos.e - maxHigh
        while f <= pos.e + maxHigh:
            if f >= 0 and f <= n \
                   and fsc.isSubsumming(basePos, fsc.StandardPosition(j,f), -1) \
                   and j >= 0:
                positions.append(fsc.StandardPosition(j,f))
            f += 1
        j += 1
        maxHigh += 1
    return positions
    

def powerSet(n, pos, basePos):
    """
Returns the powerset of the following elements

 * given position
 * positions subsumed by base position which aren't 
   subsuming the given position.
"""
    positions = getNonSubsumingPositionsForBasePosition(n, pos, basePos)
    # NOT QUITE SURE about the implementation here.
    set = []
    set.append([pos])
    for p in positions:
        s = map(lambda s: [pos] + s, powerSet(n, p, basePos))
        set += s
    return set

def getParametricListOfStates(n):
    """
For a given n-distance, this function will return all parametric
list of states for a n-distance lenvenshtein automata. This means 
that for the 1-distance lenvenshtein automata, it will return 
(as described in section 5.1):

    [[], # empty set
     [(0, 0)], # Ai
     [(0, 1)], # Bi
     [(0, 1), (1, 1)], # Ci
     [(0, 1), (1, 1), (2, 1)], # Ei
     [(0, 1), (2, 1)] # Di
""" 
    j = 0
    set = [[]]
    for f in range(n + 1):
        # Here we iterate through, minimal boundaries. The algorithm
        # below is based on this assumption: Each state is characterized 
        # first by its minimal boundary. (See Lemma 4.0.21)
        #
        # The minimal boundaries are: (0,1), (0,2), ..., (0,n). 
        # (See Definition 4.0.20 for minimal boundaries definition).
        # 
        # The second argument given to "powerSet" function is minimal 
        # boundary's right-most base position. (See Definition 4.0.18)
        #
        # Explanation of right-most base position usage:
        #
        #   We use that base position to calculate more efficiently
        #   the non-subsumed right most positions of that state. 
        #   As noted is Definition 4.0.18, a state may have several 
        #   possible base positions. However, there's only one, and only
        #   one base position which subsumes the minimal boundary and 
        #   subsumes all the other positions that can be in that state.
        #   That base position is minimal boundary's right most base 
        #   position. (NOTE: This might need a proof, or just a better
        #   explanation)
        #
        # For minimal boundary i^e, and base position j^0, then: i + e = j.
        # So, that means that for minimal boundaries: 
        # 
        #  (0,1), (0,2), ..., (0,n) 
        #
        # We will have the following correspondint base positions:
        #
        #  (1,0), (2,0), ..., (n, 0) 
        #
        set += powerSet(n, fsc.StandardPosition(j,f), fsc.StandardPosition(f, 0))
    set.sort()
    
    return set

def determineRelevantSubwordLenghts(n, states):
    """
This function register states to their possible relevant subword
lenghts. 

(See Definition 4.0.22 for "relevant subword")
    """
    s = [[] for i in range(2 * n + 2)] 
    for state in states:
        # Find the right-most position. The index of that position
        # is state's minimal lenght for a relevant subword. 
        # 
        #  For a definition of a "relevant subword for a state"
        #  see "Definitions" section on the top of the current
        #  source file.
        maxIndex = 0
        if len(state) > 0:
            rightMostPosition = max(state, key=(lambda s: s.i))
            maxIndex = rightMostPosition.i

        # Here the code will register the current state for
        # each relevant subword lenght which is at least 
        # as long as the index of the right-most position.

        # WARNING: Note that we register that state for all relevant 
        # subword of:
        #
        #   lenght >= rightMostPosition.i && lenght <= 2 * n + 1. 
        #
        # However, we over register that state. we should 
        # limit the max lenght to which we register to:
        # 
        #  maximum of each state's k = (n - e + 1)
        #  (See Definition 4.0.22).
        for index in range(maxIndex, 2 * n + 2):
            s[index].append(state)
    return s



def transition(n, profil, pos):
    # This is the transition function as described
    # by table 4.1
    #
    # Note that the current profil is the state's 
    # characteristic vector (CV), not the current 
    # position's CV. It means that position's CV 
    # is in fact:
    #
    #  profil[pos.i:min(n - e + 1, w - i)]
    #
    # (See Definition 4.0.22)
    i = pos.i
    e = pos.e
    w = len(profil)

    # Return directly if this is a match.
    if i < w and profil[i] is 1:
        return [fsc.StandardPosition(i + 1, e)]

    # Basic positions: deletion, subsitution
    positions = [fsc.StandardPosition(i, e + 1), fsc.StandardPosition(i + 1, e + 1)]
    # Addition operation:
    if i < w:
        k = fsc.positiveK(profil[i:i + min(n - e + 1, len(profil) - i)])
        if k is not None:
            positions.append(fsc.StandardPosition(i + k, e + k - 1))
    # remove positions that goes beyong profil.
    positions = filter(lambda s: s.i <= w, positions)
    # remove positions that goes beyong allowed edit mistakes
    positions = filter(lambda s: s.e <= n, positions)
    return positions


def getNextState(n, profil, state):
    # For each position, use the elementary transitions (See table 4.1).
    # Then, will have set of new positions that we will do the reduced
    # union of them. 
    #
    #  (See Lemma 4.0.21 following explanation for "reduced
    #  union" definition, and Definition 4.0.28 point 4 for explanation
    #  of the current process.
    nState = []
    for pos in state:
        nState += transition(n, profil, pos)
    return fsc.reduce(nState)
    

def genAllProfilPowerSet(n):
    set = [[] for i in range(2 * n + 2)]
    set[0] = [[]]
    for i in range(1,2 * n + 2):
        for j in [0,1]:
            set[i] += map(lambda s: [j] + s, set[i - 1])

    return set


def isStateEqual(lhs, rhs):
    lhs = fsc.reduce(lhs)
    rhs = fsc.reduce(rhs)
    lhs.sort()
    rhs.sort()
    if len(rhs) != len(lhs):
        return False

    nbStates = len(lhs)
    similar = True
    i = 0
    while similar == True and i < nbStates:
        if str(lhs[i]) != str(rhs[i]):
            similar = False
        i += 1
            
    return similar

def getSimilarState(lhs, states):
    """
    lhs and all states items must be a reduced state
    """
    foundState = None
    if lhs == []:
        foundState = ([], 0)
    i = 0
    while i < len(states) and foundState is None:
        state = copy(states[i])
        if state != []:
            state.sort()
            newLhs = copy(lhs)
            newLhs.sort()

            difference = newLhs[0].i - state[0].i
            if difference != 0:
                newLhs = map(lambda s: fsc.StandardPosition(s.i - difference, s.e),
                             lhs)
            if isStateEqual(newLhs, state):
                foundState = (state, difference)
        i += 1

    if foundState is None:
        raise "Didn't found state"
    return foundState


def genTransitions(n):
    allStates = getParametricListOfStates(n)
    statesByRelevantSubwordLenghts = determineRelevantSubwordLenghts(n, allStates)
    allProfils = genAllProfilPowerSet(n)

    transitions = [{} for i in range(2 * n + 2)]
    for profilLen in range(2 * n + 2):
        for state in statesByRelevantSubwordLenghts[profilLen]:
            for profil in allProfils[profilLen]:
                # for the current characteristic vector check what
                # is the next state. 
                nextState = getNextState(n, profil, state)

                (nextState, difference) = getSimilarState(nextState, allStates)

                profilString = str(profil)
                transitions[profilLen].setdefault(profilString, {}).setdefault(str(state), \
                 (nextState, difference))
                
    return transitions


if __name__ == "__main__":
    pprint([genTransitions(2)])
        


        
