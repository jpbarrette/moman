from fsa import *

import unittest
import copy

class FsaTests(unittest.TestCase):
    """This Class test a FSA for the sheep language defined:
    (as a regular expression) by: baaa*!
    
    so there is a list of valid strings:

    baa!
    baaa!
    baaaa!
    baaaaa!
    baaaaaa!

    and:
    (as a regular expression) by: dccc*?
    """

    def setUp(self):

        q0 = State("q0", {'b' : "q1"})
        q1 = State("q1", {'a' : "q2"})
        q2 = State("q2", {'a' : ["q2","q3"]})
        q3 = State("q3", {'a' : "q3"}, epsilon = ["q4"])
        q4 = State("q4", {'!' : "q5"})
        q5 = State("q5")

        self.nfa = Nfa([q0, q1, q2, q3, q4, q5], ["b", "a", "!"], q0, [q5])

        q0 = State("q0", {'d' : "q1"})
        q1 = State("q1", {'c' : "q2"})
        q2 = State("q2", {'c' : ["q2","q3"]})
        q3 = State("q3", {'c' : "q3"}, epsilon = ["q4"])
        q4 = State("q4", {'?' : "q5"})
        q5 = State("q5")

        self.nfa2 = Nfa([q0, q1, q2, q3, q4, q5], ["d", "c", "?"], "q0", ["q5"])
        

        q0 = State("q0", {'b' : "q1"})
        q1 = State("q1", {'a' : "q2"})
        q2 = State("q2", {'a' : "q3"})
        q3 = State("q3", {'a' : "q3", '!' : "q4"})
        q4 = State("q4")
        self.dfa = Dfa([q0, q1, q2, q3, q4], ["b", "a", "!"], "q0", ["q4"])

        q0 = State("q0", {'d' : "q1"})
        q1 = State("q1", {'c' : "q2"})
        q2 = State("q2", {'c' : "q3"})
        q3 = State("q3", {'c' : "q3", '?' : "q4"})
        q4 = State("q4")
        self.dfa2 = Dfa([q0, q1, q2, q3, q4], ["d", "c", "?"], "q0", ["q4"])

        self.fsas = [[self.dfa, self.nfa], [self.dfa2, self.nfa2]]


        
        self.recognizableTapes = [(["baa!", "baaa!", "baaaa!"], {'nfa' : self.nfa, 'dfa' : self.dfa}),
                                  (["dcc?", "dccc?", "dcccc?"], {'nfa' : self.nfa2, 'dfa' : self.dfa2})]
        self.nonRecognizableTapes = [(["ba!", "abaa!", "baaaa!!"], {'nfa' : self.nfa, 'dfa' : self.dfa}),
                                     (["dc?", "cdcc?", "dccc??", "dccc?c"], {'nfa' : self.nfa2, 'dfa' : self.dfa2})]
        self.alphabetErrorTapes = [(["cbaaa!", "bcaaa!", "baaac!", "baaa!c"], {'nfa' : self.nfa, 'dfa' : self.dfa}),
                                   (["adccc?", "daccc?", "dccca?", "dccc?a"], {'nfa' : self.nfa2, 'dfa' : self.dfa2})]
        f = lambda t: map(lambda tapeA, tapeB: tapeA + tapeB, t[0], t[1])
        self.concatenationTapes = f(map(lambda tapes: tapes[0], self.recognizableTapes))


    def testTransition(self):

        q0 = State("q0", {'a' : "q1"}, epsilon = ["q1"])
        q1 = State("q1", {'a' : "q0", 'b' : "q2"}, epsilon = ["q0"])
        q2 = State("q2", {'c' : "q2", 'd' : "q3"}, epsilon = ["q0"])
        q3 = State("q3", epsilon = ["q0"])

        nfa = Nfa([q0, q1, q2, q3], ["a", "b", "c", "d"], "q0", ["q0"])

        tests = [([q0], "a", [q0, q1]),
                 ([q1], "a", [q0, q1]),
                 ([q2], "a", [q0, q1]),
                 ([q3], "a", [q0, q1]),
                 ([q0], "b", [q0, q1, q2]),
                 ([q1], "b", [q0, q1, q2]),
                 ([q2], "b", [q0, q1, q2]),
                 ([q3], "b", [q0, q1, q2]),
                 ([q0], "c", []),
                 ([q1], "c", []),
                 ([q2], "c", [q0, q1, q2]),
                 ([q3], "c", []),
                 ([q0], "d", []),
                 ([q1], "d", []),
                 ([q2], "d", [q0, q1, q3]),
                 ([q3], "d", [])]
        for s in tests:
            states = map(lambda s: nfa.states[s.name], s[0])

            result = nfa.transition(states, s[1])
            result = map(lambda s: s.name, result)
            result.sort()
            expectedResult = map(lambda s: s.name, s[2])
            expectedResult.sort()

            errorMsg = "Transition(" + \
                       str(states) + ", " + \
                       str(s[1]) + ") == " + \
                       str(nfa.transition(states, s[1])) + " != " + \
                       str(s[2])

            self.assert_(result == expectedResult, msg = errorMsg)


        



    def testNfaKleenee(self):
        nfa = self.nfa.concatenate(self.nfa2.kleenee())

        tapes = self.concatenationTapes + \
                self.recognizableTapes[0][0] 
        for tape in tapes:
            errorMsg = 'The tape "' + tape + '" was not accepted ' \
                       "for the nfa:\n" + str(nfa) + \
                       "concatened from nfa:\n " + str(self.nfa) + \
                       "and the kleenee of:\n" + str(self.nfa2)
            self.assert_(nfa.recognize(tape), msg = errorMsg)


    def testNfaConcatenation(self):
        nfa = self.nfa.concatenate(self.nfa2)

        for tape in self.concatenationTapes:
            errorMsg = 'The tape "' + tape + '" was not accepted ' \
                       "for the nfa:\n" + str(nfa) + \
                       "concatened from nfa:\n" + str(self.nfa) + \
                       "and:\n" + str(self.nfa2)
            self.assert_(nfa.recognize(tape), msg = errorMsg)



    def testRenamingOfStates(self):
        nfa = rename(self.nfa)
        errorMsg = "The renamed FSA doesn't recognize anymore the same " \
                   "langage that the original FSA was recognizing.\n" \
                   "original:\n" + str(self.nfa) + \
                   "renamed:\n" + str(nfa)
        self.assert_(nfa.recognize("baaa!"), msg = errorMsg)



    def testNfaUnion(self):
        nfa = self.nfa.union(self.nfa2)
        tapes = self.recognizableTapes[0][0] + self.recognizableTapes[1][0]

        for tape in tapes:
            errorMsg = 'The tape "' + tape + '" was not accepted ' \
                       "for the nfa:\n" + str(nfa) + \
                       "that is an union from nfa:\n " + str(self.nfa) + \
                       "and:\n" + str(self.nfa2)
            self.assert_(nfa.recognize(tape), msg = errorMsg)



    def testUnionNfaIntegrity(self):
        nfaCopy = copy.deepcopy(self.nfa)
        nfa = self.nfa.union(self.nfa2)

        errorMsg = "The nfa used in the union operation is not the same as before.\n" + \
                   "original:\n" + str(self.nfa) + \
                   "current:\n" + str(nfaCopy)
        self.assert_(self.nfa == nfaCopy, msg = errorMsg)



    def testConcatenedNfaIntegrity(self):
        nfaCopy = copy.deepcopy(self.nfa)
        nfa = self.nfa.concatenate(self.nfa2)

        errorMsg = "The nfa used in the concatened operation is not the same as before.\n" + \
                   "original:\n" + str(self.nfa) + \
                   "current:\n" + str(nfaCopy)
        self.assert_(self.nfa == nfaCopy, msg = errorMsg)


    def testEqualityOfNfaWhenNfaIsDuplicated(self):
        nfaCopy = copy.deepcopy(self.nfa)
        errorMsg = "The copy of a nfa seem to not be equal to the original nfa.\n" \
                   "original:\n" + str(self.nfa) + \
                   "current:\n" + str(nfaCopy)
        self.assert_(self.nfa == nfaCopy, msg = errorMsg)



    def testEqualityOfStatesWhenNfaIsDuplicated(self):
        nfaCopy = copy.deepcopy(self.nfa)
        errorMsg = "The start state of a copy of a nfa seem to not be " \
                   "equal to the original nfa's start state.\n" \
                   "original:\n" + str(self.nfa.startState) + \
                   "current:\n" + str(nfaCopy.startState)
        self.assert_(self.nfa.startState == nfaCopy.startState,
                     msg = errorMsg)

    def testEqualityOfStates(self):
        state1 = State("q1", {"a" : "q2"}, epsilon = ["q2"])
        state2 = State("q2", {"a" : "q2"})

        self.assert_(state1 != state2,
                     msg = "The state q1 should not be equal to q2")
    

    def testAddDfaState(self):
        """Verifying that adding a state with more than one possibility
        for a particular character in a DFA will raise a state error.
        """
        try:
            q0 = State("q0", {"a" : ["q0", "q1"]})
            myDfa = Dfa([q0], ["a"], "q0", ["q0"])
        except StateError:
            pass
        else:
            msg = "The current dfa accepted a list of choices for a specific letter:" \
                  + self.dfa
            self.fail(msg)


    def testAlphabetError(self):
        self.tryAlphabetError('dfa')
        self.tryAlphabetError('nfa')
        

    def tryAlphabetError(self, type):
        for testingTapes in self.alphabetErrorTapes:
            for tape in testingTapes[0]:
                try:
                    testingTapes[1][type].recognize(tape)
                except AlphabetError:
                    pass
                else:
                    msg = 'The tape "' + tape + '" was accepted ' + \
                          '(but supposed to raise an alphabet error)' \
                          " for the " + type + ":\n" + str(testingTapes[1][type])
                    self.fail(msg)


    def testNonRecognizableTapes(self):
        self.tryNonRecognizableTapes('dfa')
        self.tryNonRecognizableTapes('nfa')


    def tryNonRecognizableTapes(self, type):
        for testingTapes in self.nonRecognizableTapes:
            for tape in testingTapes[0]:
                errorMsg = 'The tape "' + tape + '" was accepted ' \
                      " for the " + type + ":\n" + str(testingTapes[1][type])
                self.assert_(testingTapes[1][type].recognize(tape) == False, msg = errorMsg)


    def testRecognizableTapes(self):
        self.tryRecognizableTapes('dfa')
        self.tryRecognizableTapes('nfa')
        

    def tryRecognizableTapes(self, type):
        for testingTapes in self.recognizableTapes:
            for tape in testingTapes[0]:
                errorMsg = 'The tape "' + tape + '" was not accepted ' \
                      " for the " + type + ":\n" + str(testingTapes[1][type])
                self.assert_(testingTapes[1][type].recognize(tape), msg = errorMsg)



    def testComplement(self):
        q0 = State("q0", {"a" : "q1", "b" : "q1"})
        q1 = State("q1", {"a" : "q2", "b" : "q2"})
        q2 = State("q2", {"a" : "q2", "b" : "q2"})

        myDfa = Dfa([q0, q1, q2], ["a", "b"], "q0", "q1")
        
        complementDfa = myDfa.complement()
        self.assert_(complementDfa.recognize("a") == False,
                     msg = "The complement operation over a DFA should not recognize " + \
                     "a string recognized by the original DFA")
        self.assert_(complementDfa.recognize("aa"),
                     msg = "The complement operation over a DFA should recognize " + \
                     "any string not recognized by the original DFA")

        complementNfa = self.nfa.complement()
        self.assert_(complementNfa.recognize("baaa!") == False,
                     msg = "The complement operation over a NFA should not recognize " + \
                     "a string recognized by the original NFA")
        self.assert_(complementNfa.recognize("aaabb"),
                     msg = "The complement operation over a NFA should recognize " + \
                     "any string not recognized by the original NFA")


    def testPowerSet(self):
        self.assert_(powerSet(["q1", "q2", "q3"]) == [[],
                                        ['q1'],
                                        ['q2'],
                                        ['q3'],
                                        ['q1', 'q2'],
                                        ['q1', 'q3'],
                                        ['q2', 'q3'],
                                        ['q1', 'q2', 'q3']],
                     msg = "The power set function return an invalid power set")


    def testReverse(self):
        dfa = self.dfa.reverse()

        self.assert_(dfa.recognize("!aaab"))
        self.assert_(dfa.recognize("!aaaab"))
        self.assert_(dfa.recognize("!aab"))
        self.assert_(dfa.recognize("aaab") == False)

    def testDeterminize(self):
        dfa = self.nfa.determinize()
        self.assert_(dfa.recognize("baaa!"),
                     msg = "The determinized NFA doesn't recognize the same language")
        self.assert_(dfa.recognize("baab!") == False,
                     msg = "The determinized NFA doesn't recognize the same language")


    def testStateValidity(self):
        """Testing if adding an invalid transition within a state,
        will raise an error.
        """
        q0 = State("q0", {"a" : "q1", "b" : "q1", "c" : "q1"})
        try:
            dfa = Dfa([q0], ["a", "b", "c"], q0, q0)
        except StateError, e:
            pass
        except:
            msg = "The current DFA accepted a transition's state " + \
                  "that is not part of the DFA"
            self.fail(msg)


    def testMinimization(self):
        q0 = State("q0", {"a" : ["q1", "q3"]})
        q1 = State("q1", {"a" : "q2", "b" : "q2"})
        q2 = State("q2", {"a" : "q2", "b" : "q2"})
        q3 = State("q3", {"a" : "q2", "b" : "q4"})
        q4 = State("q4", {"a" : "q4", "b" : "q4"})

        nfa = Nfa([q0, q1, q2, q3, q4], ["a", "b"], "q0", ["q2", "q4"])

        dfa = nfa.minimize()
        
        msg1 = "The minimization of the NFA should accept this string."
        msg2 = "The minimization of the DFA should not accept this string."

        self.assert_(dfa.recognize("aa"), msg = msg1)
        self.assert_(dfa.recognize("ab"), msg = msg1)
        self.assert_(dfa.recognize("aaa"), msg = msg1)
        self.assert_(dfa.recognize("aab"), msg = msg1)
        self.assert_(dfa.recognize("aba"), msg = msg1)
        self.assert_(dfa.recognize("abb"), msg = msg1)
        self.assert_(dfa.recognize("aaaa"), msg = msg1)
        self.assert_(dfa.recognize("aaab"), msg = msg1)
        self.assert_(dfa.recognize("aaba"), msg = msg1)
        self.assert_(dfa.recognize("aabb"), msg = msg1)
        self.assert_(dfa.recognize("abaa"), msg = msg1)
        self.assert_(dfa.recognize("abab"), msg = msg1)
        self.assert_(dfa.recognize("abba"), msg = msg1)
        self.assert_(dfa.recognize("abbb"), msg = msg1)
        self.assert_(dfa.recognize("a") == False, msg = msg2)
        self.assert_(dfa.recognize("b") == False, msg = msg2)
        self.assert_(dfa.recognize("ba") == False, msg = msg2)
        self.assert_(dfa.recognize("bb") == False, msg = msg2)
        self.assert_(dfa.recognize("baa") == False, msg = msg2)
        self.assert_(dfa.recognize("bab") == False, msg = msg2)
        self.assert_(dfa.recognize("bba") == False, msg = msg2)
        self.assert_(dfa.recognize("bbb") == False, msg = msg2)
        self.assert_(dfa.recognize("baaa") == False, msg = msg2)
        self.assert_(dfa.recognize("baab") == False, msg = msg2)
        self.assert_(dfa.recognize("baba") == False, msg = msg2)
        self.assert_(dfa.recognize("babb") == False, msg = msg2)
        self.assert_(dfa.recognize("bbaa") == False, msg = msg2)
        self.assert_(dfa.recognize("bbab") == False, msg = msg2)
        self.assert_(dfa.recognize("bbba") == False, msg = msg2)
        self.assert_(dfa.recognize("bbbb") == False, msg = msg2)
        

    def testDifference(self):
        """Testing the difference operation over DFAs.
        """

        q0 = State("q0", {"a" : "q1", "b" : "q1", "c" : "q1"})
        q1 = State("q1", {"a" : "q2", "b" : "q2", "c" : "q2"})
        q2 = State("q2", {"a" : "q2", "b" : "q2", "c" : "q2"})

        myDfa = Dfa([q0, q1, q2], ["a", "b", "c"], "q0", "q1")

        q3 = State("q3", {"a" : "q4", "b" : "q5"})
        q4 = State("q4", {"a" : "q5", "b" : "q5"})
        q5 = State("q5", {"a" : "q5", "b" : "q5"})

        myDfa2 = Dfa([q3, q4, q5], ["a", "b"], "q3", "q4")
        dfa = myDfa.difference(myDfa2)

        msg1 = "The difference of the two DFAs should accept this string."
        msg2 = "The difference of the two DFAs should not accept this string."

        self.assert_(dfa.recognize("b"), msg = msg1)
        self.assert_(dfa.recognize("bb") == False, msg = msg2)
        self.assert_(dfa.recognize("bbb") == False, msg = msg2)
        self.assert_(dfa.recognize("ab") == False, msg = msg2)
        self.assert_(dfa.recognize("bab") == False, msg = msg2)
        self.assert_(dfa.recognize("aab") == False, msg = msg2)
        self.assert_(dfa.recognize("ba") == False, msg = msg2)
        self.assert_(dfa.recognize("aba") == False, msg = msg2)
        self.assert_(dfa.recognize("bba") == False, msg = msg2)
        self.assert_(dfa.recognize("a") == False, msg = msg2)
        self.assert_(dfa.recognize("aa") == False, msg = msg2)
        self.assert_(dfa.recognize("aaa") == False, msg = msg2)


    def testIntersection(self):
        """Testing the intersection operation over DFAs.
        """

        q0 = State("q0", {"a" : "q1", "b" : "q1"})
        q1 = State("q1", {"a" : "q2", "b" : "q2"})
        q2 = State("q2", {"a" : "q2", "b" : "q2"})

        myDfa = Dfa([q0, q1, q2], ["a", "b"], "q0", "q1")

        q3 = State("q3", {"a" : "q4", "b" : "q5"})
        q4 = State("q4", {"a" : "q5", "b" : "q5"})
        q5 = State("q5", {"a" : "q5", "b" : "q5"})

        myDfa2 = Dfa([q3, q4, q5], ["a", "b"], "q3", "q4")
        
        dfa = myDfa.intersection(myDfa2)
        self.assert_(dfa.recognize("aaaa") == False)
        self.assert_(dfa.recognize("aa") == False)
        self.assert_(dfa.recognize("a") == True)
        self.assert_(dfa.recognize("bab") == False)


        #testing intersection with DFAs with different alphabets
        q0 = State("q0", {"a" : "q1"})
        q1 = State("q1", {"a" : "q2"})
        q2 = State("q2", {"a" : "q2"})

        #accepting only one "a"
        myDfa = Dfa([q0, q1, q2], ["a"], "q0", "q1")

        q3 = State("q3", {"b" : "q4"})
        q4 = State("q4", {"b" : "q5"})
        q5 = State("q5", {"b" : "q5"})

        #accepting only one "b"
        myDfa2 = Dfa([q3, q4, q5], ["b"], "q3", "q4")

        dfa = myDfa.intersection(myDfa2)
        self.assert_(dfa.recognize("aaaa") == False)
        self.assert_(dfa.recognize("aa") == False)
        self.assert_(dfa.recognize("a") == False)
        self.assert_(dfa.recognize("b") == False)
        self.assert_(dfa.recognize("bab") == False)

        

    
if __name__ == "__main__":
    unittest.main()
