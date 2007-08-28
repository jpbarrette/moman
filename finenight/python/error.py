class Error:
    def __init__(self, string):
        self.string = string

    def __str__(self):
        return self.string




class StateError(Error):
    """This error is raised when a state is invalid"""




class AlphabetError(Error):
    """This error is raised when the alphabet of a FSA is invalid"""




class ConstructionError(Error):
    """This error is raised when we encounter a problem when
    construction a FSA.
    """

class NotImplemented(Error):
    """This error is raised when the implementation of the function
    is incomplete
    """
