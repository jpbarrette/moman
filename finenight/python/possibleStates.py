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


def determineRelevantSubwordLenghts(n, state):
    """
This function register states to their possible relevant subword
lenghts. 

(See Definition 4.0.22 for "relevant subword")
    """
    # Find the right-most position. The index of that position
    # is state's minimal lenght for a relevant subword. 
    # 
    #  For a definition of a "relevant subword for a state"
    #  see "Definitions" section on the top of the current
    #  source file.
    minProfilLen = 0
    if len(state) > 0:
        rightMostPosition = max(state, key=lambda s:s.i)
        minProfilLen = rightMostPosition.i

    return (minProfilLen, 2 * n + 1)


def transition(n, profil, pos, withTransitions):
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

    positions = []
    if pos.isTransposition:
        # if we are a transposition position, the only
        # accepting position if profil is [0, 1]
        if profil[i] == 1:
            return [fsc.StandardPosition(i + 2, e)]
        else:
            return positions

    if withTransitions and (w - i) >= 2 and profil[i:i + 2] == [0, 1]:
        positions += [fsc.TPosition(i, e + 1)]

    # Return directly if this is a match.
    if i < w and profil[i] is 1:
        return [fsc.StandardPosition(i + 1, e)]

    # Basic positions: deletion, subsitution
    positions += [fsc.StandardPosition(i, e + 1), fsc.StandardPosition(i + 1, e + 1)]

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


def getNextState(n, profil, state, withTransitions):
    # For each position, use the elementary transitions (See table 4.1).
    # Then, will have set of new positions that we will do the reduced
    # union of them. 
    #
    # (See Lemma 4.0.21 following explanation for "reduced union" 
    # definition, and Definition 4.0.28 point 4 for explanation of 
    # the current process).
    nextState = []
    for pos in state:
        nextState += transition(n, profil, pos, withTransitions)
        
    difference = 0
    if len(nextState) > 0:
        nextState = fsc.reduce(nextState, n)
        difference = nextState[0].i - state[0].i

        if difference > 0:
            for pos in nextState:
                pos.i = pos.i - difference

    return nextState, difference


def genAllProfilPowerSet(n):
    set = [[] for i in range(2 * n + 2)]
    set[0] = [[]]
    for i in range(1,2 * n + 2):
        for j in [0,1]:
            set[i] += map(lambda s: [j] + s, set[i - 1])

    return set


def genTransitions(n, withTransitions = True):
    allProfils = genAllProfilPowerSet(n)
    transitions = [{} for i in range(2 * n + 2)]
    
    # Add the initial state
    processedStates = []
    unprocessedStates = [[fsc.StandardPosition(0,0)]]
    while len(unprocessedStates) > 0:
        state = unprocessedStates.pop()
        processedStates.append(state)

        profilLenMin, profilLenMax = determineRelevantSubwordLenghts(n, state)
        for profilLen in range(profilLenMin, profilLenMax + 1):
            for profil in allProfils[profilLen]:
                # for the current characteristic vector check what
                # is the next state. 
                nextState, difference = getNextState(n, profil, state, withTransitions)
                transitions[profilLen].setdefault(str(profil), {}).setdefault(str(state), (nextState, difference))
                if nextState != [] and not nextState in processedStates and not nextState in unprocessedStates:
                    unprocessedStates.append(nextState)
                
    return transitions


if __name__ == "__main__":
    pprint([genTransitions(2)])
        


        
