from iadfa import *
import pdb
from pprint import pprint


def test():
    dfa = IncrementalAdfa(["aient",
                           "ais",
                           "ait",
                           "ant"])
    print dfa
    
    
if __name__ == "__main__":
    test()
