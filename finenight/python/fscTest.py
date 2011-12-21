import fsc
import possibleStates

import unittest
import copy

class FsaTests(unittest.TestCase):
    def testSubsumming(self):
        """
        Test if the subsumming test is okay.
        """
        areSubsummed = [[(1,0), (0,1)],
                        [(1,0), (1,1)],
                        [(1,0), (2,1)],
                        [(1,0), (0,2)],
                        [(1,0), (1,2)],
                        [(1,0), (2,2)],
                        [(1,0), (3,2)],
                        [(1,0), (0,3)],
                        [(1,0), (1,3)],
                        [(1,0), (2,3)],
                        [(1,0), (3,3)],
                        [(1,0), (4,3)],
                        [(2,0), (1,1)],
                        [(2,0), (2,1)],
                        [(2,0), (3,1)],
                        [(2,0), (0,2)],
                        [(2,0), (1,2)],
                        [(2,0), (2,2)],
                        [(2,0), (3,2)],
                        [(2,0), (4,2)],
                        [(2,0), (0,3)],
                        [(2,0), (1,3)],
                        [(2,0), (2,3)],
                        [(2,0), (3,3)],
                        [(2,0), (4,3)],
                        [(2,0), (5,3)]]
        n = 1
        for entry in areSubsummed:
            errorMsg = "The entry " + str(entry[0]) + " is supposed to subsume " +\
                       "the entry " + str(entry[1]) + " but it isn't"
            self.assert_(fsc.isSubsumming(fsc.StandardPosition(entry[0][0], entry[0][1]), \
                                          fsc.StandardPosition(entry[1][0], entry[1][1]), n), msg = errorMsg)



    def testNotSubsumming(self):
        """
        Test if the subsumming test is okay.
        """
        areSubsummed = [[(1,0), (1,0)],
                        [(1,0), (0,0)],
                        [(1,0), (2,0)],
                        [(1,0), (3,0)],
                        [(1,0), (3,1)],
                        [(1,0), (4,0)],
                        [(1,0), (4,1)],
                        [(1,0), (4,2)],
                        [(1,0), (5,0)],
                        [(1,0), (5,1)],
                        [(1,0), (5,2)],
                        [(1,0), (5,3)],
                        [(1,0), (6,0)],
                        [(1,0), (6,1)],
                        [(1,0), (6,2)],
                        [(1,0), (6,3)],
                        [(1,0), (6,4)]]
        n = 1
        for entry in areSubsummed:
            errorMsg = "The entry " + str(entry[0]) + " is not supposed to subsume " +\
                       "the entry " + str(entry[1]) + " but it is"
            self.assert_(not fsc.isSubsumming(fsc.StandardPosition(entry[0][0], entry[0][1]), \
                                              fsc.StandardPosition(entry[1][0], entry[1][1]), n), msg = errorMsg)

##    def testComputeLevAuto(self):
##        print fsc.computeLevAuto(8,1)

##    def testGenCharVec(self):
##        print fsc.genCharVectors(3)
        
    
if __name__ == "__main__":
    unittest.main()
