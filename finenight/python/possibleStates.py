import fsc
from copy import copy
from pdb import set_trace
from pprint import pprint


     
def mapStateToLetter(state):
    return str(state)
    m = {"[]" : "None",
         "[(0, 0)]" : "A",
         "[(0, 1)]" : "B",
         "[(0, 1), (1, 1)]" : "C",
         "[(0, 1), (1, 1), (2, 1)]" : "E",
         "[(0, 1), (2, 1)]" : "D"}
    state = copy(state)
    state.sort()
    return m[str(state)]


def getRightNonSubsumingPositions(n, pos, basePos):
    positions = []
    j = pos.i + 1
    maxDistance = basePos.i + n + 1
    maxHigh = 0
    while j < maxDistance:
        f = pos.e - maxHigh
        while f <= pos.e + maxHigh:
            if f >= 0 and f <= n \
                   and fsc.isSubsumming(basePos, fsc.StandardPosition(j,f)) \
                   and j >= 0:
                positions.append(fsc.StandardPosition(j,f))
            f += 1
        j += 1
        maxHigh += 1
    return positions
    

def powerSet(n, pos, basePos):
    positions = getRightNonSubsumingPositions(n, pos, basePos)
    set = []
    set.append([pos])
    for p in positions:
        s = map(lambda s: [pos] + s, powerSet(n, p, basePos))
        set += s
    return set

def possibleStates(n):
    basePosIndex = 0
    j = 0
    set = [[]]
    for f in range(n + 1):
        set += powerSet(n, fsc.StandardPosition(j,f), fsc.StandardPosition(basePosIndex, 0))
        basePosIndex += 1
    set.sort()
    
    return set

def getMaxIndexOfState(state):
    maxIndex = 0
    for pos in state:
        if pos.i > maxIndex:
            maxIndex = pos.i
    return maxIndex


def diviseStatesPerMaxIndex(n, states):
    s = [[] for i in range(2 * n + 2)] 
    for state in states:
        maxIndex = getMaxIndexOfState(state)
        for index in range(maxIndex, 2 * n + 2):
            s[index].append(state)
    return s





def transition(n, profil, pos):
    i = pos.i
    e = pos.e
    w = len(profil)
    if 0 <= pos.e and pos.e <= n - 1:
        if i <= (w - 2):
            if profil[i] is 1:
                return [fsc.StandardPosition(i + 1, e)]
            k = fsc.positiveK(profil[i:min(n - e + 1,
                                           len(profil) - i)])
            if k is not None:
                return [fsc.StandardPosition(i, e + 1),
                        fsc.StandardPosition(i + 1, e + 1),
                        fsc.StandardPosition(i + k, e + k - 1)]
            else:
                return [fsc.StandardPosition(i, e + 1),
                        fsc.StandardPosition(i + 1, e + 1)]
        if i == (w - 1):
            if profil[i] is 1:
                return [fsc.StandardPosition(i + 1, e)]
            else:
                return [fsc.StandardPosition(i, e + 1),
                        fsc.StandardPosition(i + 1, e + 1)]
        if i == w:
            return [fsc.StandardPosition(i, e + 1)]
    else:
        if i <= w - 1:
            if profil[i] == 1:
                return [fsc.StandardPosition(i + 1, n)]
            else:
                return []
        if i == w:
            return []
    set_trace()
    return []

def baseSubword(input, n, i, e):
    k = n - e + 1
    return input[:k]


def getNextState(n, profil, state):
    w = len(profil)
    nState = []
    for pos in state:
        nState += transition(n, profil, pos)
    return nState
    

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
    """lhs and all states items must be a reduced state"""
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
            if str(state) == str([(2,1), (0,1)]) and \
                   str(newLhs) == str([(2,1), (3,1)]):
                set_trace()
            if isStateEqual(newLhs, state):
                foundState = (state, difference)
        i += 1

    if foundState is None:
        set_trace()
    return foundState


def genTransitions(n):
    allStates = possibleStates(n)
    states = diviseStatesPerMaxIndex(n, allStates)
    allProfils = genAllProfilPowerSet(n)
    transitions = [{} for i in range(2 * n + 2)]
    for profilLen in range(2 * n + 2):
        for state in states[profilLen]:
            for profil in allProfils[profilLen]:
                profilString = str(profil)
                nextState = fsc.reduce(getNextState(n, profil, state))
                (nextState, difference) = getSimilarState(nextState, allStates)
                transitions[profilLen].setdefault(profilString, {}).setdefault(str(mapStateToLetter(state)), (nextState, difference))
                
    return transitions


if __name__ == "__main__":
    n = 2
    pprint([genTransitions(n)])
        


        


