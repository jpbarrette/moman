


def ed( X, Y ):
    i = len(X) - 1
    j = len(Y) - 1
    
    #last character are same
    while True:
        if i >=0 and j >=0: 
            if X[i] == Y[j]:
                dis = ed( X[:i], Y[:j])
                break


        #last two characters are transposed
        if i >= 1 and j >= 1:
            if X[-2] == Y[-1] and X[-1] == Y[-2]:
                dis = 1 + min( ed( X[:-2],Y[:-2] ),
                               ed( X, Y[:-1] ),
                               ed( X[:-1], Y ) )
                break

        #test if there's some symbol left to test
        if i == -1:
            dis = j + 1
            break
        if j == -1:
            dis = i + 1
            break

        dis = 1 + min( ed( X[:-1], Y[:-1] ),
                       ed( X, Y[:-1] ),
                       ed( X[:-1], Y ) )
        break
    return dis


def cuted( X, Y, t ):
    m = len(X)
    n = len(Y)
    
    l = max(1,n-t)
    u = max(m,n+t)

    distance = None
    for i in range(l,u):
        d = ed(X[:i],Y)
        if distance is None or \
           distance > d:
            distance = d

    return distance


class ErrorTolerantRecognizer:
    """
    This class is meant to recognize erroneous words.
    """
    def __init__(self, n = None, transitionsStates = None):
        """
        This class is only a leveinstein distance one,
        so we don't care about distance.
        """
        pass
    
    def recognize(self, word, fsa, t = 2):
        words = []
        states = []
        states.append(("", fsa.startState))
        while len(states):
            (Yp, qi) = states.pop()
            qi = fsa.states[qi]
            for (qj,a) in map(lambda (a,s): (s,a), qi.transitions.items()):
                Y = Yp + a
                if cuted(word, Y, t) <= t:
                    states.append((Y,qj))
                if ed(word, Y) <= t and qj in fsa.finalStates:
                    words.append(Y)
        return words


if __name__ == "__main__":
    import sys
    import pickle
    from iadfa import IncrementalAdfa
    sys.argv = sys.argv[1:]

    etr = ErrorTolerantRecognizer()
    pFile = "test4.pickle"
    if "--gen" in sys.argv:
        sys.argv.remove("--gen")
        file = open("test.dico", "r")
        words = []
        for word in file:
            if word[-1] == "\n":
                word = word[:-1]
            words.append(word)
        words.sort()
        iadfa = IncrementalAdfa(words, sorted = True)
        pickle.dump(iadfa, open(pFile,"w")) 
    else:
        iadfa = pickle.load(open(pFile, "r"))
    if "--export" in sys.argv:
        sys.argv.remove("--export")
        iadfa.graphVizExport("test4.dot")

    etr.recognize(sys.argv[0],iadfa,2)
