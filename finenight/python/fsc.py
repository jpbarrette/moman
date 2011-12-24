import copy
import types
import pdb


class Position:
    def __init__(self, i, e, isTransposition):
        self.isTransposition = isTransposition
        self.i = i
        self.e = e
        
    def __str__(self):
        val = str((self.i, self.e))
        if self.isTransposition:
            val = 't' + val
        return val

    def __repr__(self):
        return str(self)

    def __eq__(lhs, rhs):
        return lhs.i == rhs.i and \
               lhs.e == rhs.e and \
               lhs.isTransposition == rhs.isTransposition

    def __lt__(lhs, rhs):
        if lhs.i < rhs.i:
            return True

        if lhs.i >= rhs.i:
            return False

        # consider a standard position as lower than a transposition position
        if not lhs.isTransposition and rhs.isTransposition:
            return True
        
        if lhs.isTransposition != rhs.isTransposition:
            return False

        if lhs.e < rhs.e:
            return True

        if lhs.e > rhs.e:
            return False


        return False


def StandardPosition(i, e):
    return Position(i, e, False)

def TPosition(i, e):
    return Position(i, e, True)


def isSubsumming(subsumming, subsummee, n):
    i, e = subsumming.i, subsumming.e
    j, f = subsummee.i, subsummee.e

    # true if i and j are t-positions
    it = subsumming.isTransposition
    jt = subsummee.isTransposition

    # see 7.1.3
    if it:
        if jt:
            # 4. position it^e subsumes jt^f iff f > e and i = j
            if it and jt and f > e and i == j:
                return True
        else:
            # 3. position it^e subsumes j^f iff n = f > e and i = j
            if n == f and f > e and i == j:
                return True 
            
    else:
        if jt:
            # 2. position i^e subsumes jt^f iff f > e and |j-(i-1)| <= f - e
            if f > e and (abs(j - (i - 1)) <= (f - e)):
                return True
        else:
            # 1. position i^e subsumes j^f iff e < f and |j-i| <= f - e
            if e < f and (abs(j - i) <= (f - e )):
                return True
        
    return False



def reduce(M, n):
    # Process entries by number of errors.
    # This is because a entry can only be subsumed
    # by entries with less errors.
    items = {}
    for entry in M:
        l = items.setdefault(entry.e, [])
        item = [entry, False]
        if item not in l:
            l.append(item)

    keys = items.keys()
    keys.sort()
    for eIndex in range(len(keys)):
        e = keys[eIndex]
        for item in items[e]:
            if item[1] is False:
                pos = item[0]
                for f in keys[eIndex + 1:]:
                    for jIndex in range(len(items[f])):
                        otherItem = items[f][jIndex]
                        if otherItem[1] is False:
                            otherPos = otherItem[0]
                            if isSubsumming(pos, otherPos, n):
                                otherItem[1] = True
        items[e] = filter(lambda j: not j[1], items[e])
    union = []
    for key in items:
        for item in items[key]:
            union.append(item[0])
    union.sort()
            
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


def union(M, N, n):
    if type(M) is not types.ListType:
        M = [M]
    if type(N) is not types.ListType:
        N = [N]

    return reduce(M + N, n)


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


