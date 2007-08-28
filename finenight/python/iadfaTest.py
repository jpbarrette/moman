from iadfa import IncrementalAdfa

f = ["append", "appendice", "bappend"]

fsa = IncrementalAdfa(f, sorted = True)
fsa.graphVizExport("test.dot")
