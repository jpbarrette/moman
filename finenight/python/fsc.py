import copy
import types
import pdb


class Position:
    def __init__(self, i, e):
        self.i = i
        self.e = e
        
    def __str__(self):
        return str((self.i, self.e))

    def __repr__(self):
        return str(self)

    def __eq__(lhs, rhs):
        return lhs.i == rhs.i and \
               lhs.e == lhs.e

    def __lt__(lhs, rhs):
        if lhs.i < rhs.i:
            return True

        if lhs.i == rhs.i:
            if lhs.e < rhs.e:
                return True
            else:
                return False

        return False
    

class StandardPosition(Position):
    pass

class TPosition(Position):
    def __str__(self):
        return 't' + str((self.i, self.e))

    def __repr__(self):
        return str(self)



def isSubsumming(subsumming, subsummee, n):
    i, e = subsumming.i, subsumming.e
    j, f = subsummee.i, subsummee.e

    # true if i and j are t-positions
    it = isinstance(subsumming, TPosition)
    jt = isinstance(subsummee, TPosition)

    # see 7.1.3

    # 1. position i^e subsumes j^f iff e < f and |j-i| <= f - e
    if e < f and (abs(j - i) <= (f - e )):
        return True
    
    # 2. position i^e subsumes jt^f iff f > e and |j-(i-1)| <= f - e
    if not it and jt and f > e and (abs(j - (i - 1)) <= (f - e)):
        return True

    # 3. position it^e subsumes j^f iff n = f > e and i = j
    if it and jt and n == f and f > e and i == j:
        return True 

    # 4. position it^e subsumes jt^f iff f > e and i = j
    if it and jt and f > e and i == j:
        return True

    return False




def reduce(m):
    n = {}
    for entry in m:
        l = n.setdefault(entry.e, [])
        item = (entry.i, False)
        if item not in l:
            l.append(item)

    keys = n.keys()
    keys.sort()
    for eIndex in range(len(keys)):
        e = keys[eIndex]
        for i in n[e]:
            if i[1] is False:
                i = i[0]
                for f in keys[eIndex + 1:]:
                    for jIndex in range(len(n[f])):
                        j = n[f][jIndex][0]
                        if n[f][jIndex][1] is False:
                            if isSubsumming(StandardPosition(i,e),
                                            StandardPosition(j,f), -1):
                                n[f][jIndex] = (j, True)
        n[e] = filter(lambda j: not j[1], n[e])
    union = []
    for key in n:
        for item in n[key]:
            union.append(StandardPosition(item[0], key))
            
    return union


def subword(input, n, i, e):
    w = len(input)
    k = min(2*n - e + 1, w - i)
    return input[i:i + k]


def positiveK(cString):
    i = 0
    while i < len(cString) and  cString[i] != 1:
        i = i + 1
    if i == len(cString):
        return None
    else:
        return i + 1


def transition(input, x, n, i, e):
    w = len(input)
    inputSubword = subword(input, n, i, e)
    cString = characterizedVector(x, inputSubword)
    if 0 <= e and e <= n - 1:
        if i <= (w - 2):
            if cString[0] is 1:
                return [(i + 1, e)]
            k = positiveK(cString)
            if k is not None:
                return [(i, e + 1),
                        (i + 1, e + 1),
                        (i + k, e + k - 1)]
            else:
                return [(i, e + 1),
                        (i + 1, e + 1)]
        if i == (w - 1):
            if cString[0] is 1:
                return [(i + 1, e)]
            else:
                return [(i, e + 1), (i + 1, e + 1)]
        if i == w:
            return [(i, e + 1)]
    else:
        if i <= w - 1:
            if cString[0] == 1:
                return [(i + 1, n)]
            else:
                return []
        if i == w:
            return []

    
            

def union(M, N):
    if type(M) is not types.ListType:
        M = [M]
    if type(N) is not types.ListType:
        N = [N]

    return reduce(M + N)


def profil( inputWord ):
    characters = {}
    cVector = []
    currentSymbol = 1
    for c in inputWord:
        if c not in characters:
            characters[c] = currentSymbol
            currentSymbol += 1
        cVector.append(characters[c])
    return cVector

    
def characterizedVector( character, inputWord ):
    cVector = []
    for c in inputWord:
        if c == character:
            cVector.append(1)
        else:
            cVector.append(0)
    return cVector


def genCharVectors(l):
    vectors = [[0] * l]
    for i in range(pow(2, l) - 1):
        vectors.append(addone(vectors[-1]))
    return vectors


def addone(vec):
    if len(vec) == 0:
        return []
    if vec[0] == 0:
        return [1] + vec[1:]
        
    if vec[0] == 1:
        return [0] + addone(vec[1:])        




def isLikeStates(state, lowerStates):

    isLike = False
    i = 0
    while i < len(lowerStates) and isLike == False:
        lowerState = lowerStates[i]
        state = copy.copy(state)
        state.sort()

        difference = state[0][0] - lowerState[0][0]
        for index in range(len(state)):
            state[index] = state[index][0] - difference, state[index][1]

        if state == lowerState:
            isLike = True

        i += 1

    return isLike
    

def delta( n, (stateType, index), character, input, states ):
    cv = characterizedVector( character, input )[:(2 * n + 1)]
    l = len(cv)
    w = states[l]
    cv = str(cv)

    state = None
    if w.has_key(cv) and w[cv].has_key(str(stateType)):
        state = w[cv][str(stateType)]
        state = (state[0], state[1] + index)
    return state


def final(n, state, index, wordLen):
    isFinal = False

    j = 0
    while j < len(state) and isFinal == False:
        i = state[j].i + index
        e = state[j].e
        if wordLen - i + e <= n:
            isFinal = True
        j += 1
    return isFinal
    return False


    
class ErrorTolerantRecognizer:
    def __init__(self, n, transitionsStates = None):
        if transitionsStates is None:
            transitionsStates = handCraftedStates
        self.transitionsStates = transitionsStates
        self.n = n
            
    def recognize( self, word, fsa):
        words = []
        wordLen = len(word)
        
        states = [("", fsa.startState, (str([(0,0)]), 0))]
        while len(states):
            (V, q, M) = states.pop()
            for (x, q1) in fsa.states[q].transitions.items():
                mPrime = delta( self.n, M, x, word[M[1]:], self.transitionsStates )
                if mPrime[0] != []:
                    V1 = V + x
                    states.append((V1, q1, mPrime))
            if q in fsa.finalStates and final(self.n, M[0], M[1], wordLen):
                words.append(V)
        return words


