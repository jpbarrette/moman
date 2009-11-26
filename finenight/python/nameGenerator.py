
class IndexNameGenerator:
    """Renaming states with this class is not stable, that is,
    it's not sure that renaming the FSA will give allways the
    same result.
    """
    def __init__(self):
        self.index = 0

    def generate(self):
        name = "q" + str(self.index)
        self.index += 1
        return name

class PlainIndexNameGenerator:
    """Renaming states with this class is not stable, that is,
    it's not sure that renaming the FSA will give allways the
    same result.
    """
    def __init__(self):
        self.index = 0

    def generate(self):
        name = str(self.index)
        self.index += 1
        return name

