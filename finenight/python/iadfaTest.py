from iadfa import IncrementalAdfa

f = ["append", "appendice", "bappend", "bappendice", "bateau", "batis", "batise", "batisise", "brateau", "cracher", "crateau", "crateauzise", "croteau", "croteaunize"]

fsa = IncrementalAdfa(f, sorted = True)
fsa.graphVizExport("test.dot")
