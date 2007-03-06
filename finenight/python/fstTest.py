from fst import Fst

import unittest


class FstTests(unittest.TestCase):
    def setUp(self):
        
        states = [(0, 'a', 'b', 1),
                  (0, 'b', 'a', 1),
                  (1, 'b', 'a', 1)]

        self.fst = Fst(states, ['a', 'b'], ['a', 'b'], 0, 1)

    def testInverse(self):
        """
        This function is testing if the inverse is as it should be.
        """
        states = [(0, 'b', 'a', 1),
                  (0, 'a', 'b', 1),
                  (1, 'a', 'b', 1)]

        inverseFst = Fst(states, ['a', 'b'], ['a', 'b'], 0, 1)
        realInverseFst = self.fst.inverse()
        errorMsg = "\nThe inverse of fst:\n" + str(self.fst) + "\n\n" \
                   "should be like:\n" + str(inverseFst) + "\n\n" \
                   "but it's like:\n" + str(realInverseFst)
        self.assert_(inverseFst == realInverseFst, msg = errorMsg)


if __name__ == "__main__":
    unittest.main()
