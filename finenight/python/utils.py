from fsa import *

import copy

def create(start, finals, edges):
    states = {}
    for e in edges:
        if e[1] is None:
            states.setdefault(e[0], ({}, []))[1].append(e[2])
        else:
            states.setdefault(e[0], ({}, []))[0][e[1]] = e[2]
        states.setdefault(e[2], ({}, []))
    
    states = map(lambda s: State(s[0], s[1][0], epsilon = s[1][1]), states.items())
    alphabet = []
    for s in states:
        for t in s.transitions:
            if t not in alphabet:
                alphabet.append(t)

    states_map = dict(map(lambda s: (s.name, s), states))
        
    return Nfa(states, alphabet, states_map[start], map(lambda s: states_map[s], finals))


def union(lhs, rhs):
    lhs, rhs = binaryOperationRenaming(lhs, rhs, True, None)

    
    new = Nfa(lhs.states.values() + rhs.states.values(),
              lhs.alphabet + filter(lambda s: s not in lhs.alphabet,
                                    rhs.alphabet),
              lhs.startState,
              rhs.finalStates + lhs.finalStates)
    new.states[new.startState].epsilon.append(rhs.startState)
    return new

# this function will produce an fsa that will accept a mininum
# of "start" of that fsa to less than "end" fsa
def repeat(lhs, start, end):
    
    # add the optional ones.
    optional = copy.deepcopy(lhs)
    for final in optional.finalStates:
        optional.states[optional.startState].epsilon.append(final)

    if start > 0:
        new = copy.deepcopy(lhs)
        for i in range(1, start):
            new = new.concatenate(lhs)
    else:
        new = optional
        end = end - 1
    

    for i in range(start, end):
        new = new.concatenate(optional)

    return new


# This function will remove the deadend states. 
# This makes the FSA cleaner to display.
def clean(fsa):
    deadends = []
    for label,state in fsa.states.items():
        if label not in fsa.finalStates:
            destinations = []
            for dest in state.transitions.values():
                for d in dest:
                    if d not in destinations:
                        destinations.append(d)
            if label in destinations:
                destinations.remove(label)
            if len(destinations) == 0:
                deadends.append(label)
    
    for label,state in fsa.states.items():
        for input,dest in state.transitions.items():
            for d in dest:
                if d in deadends:
                    dest.remove(d)
            if len(dest) == 0:
                del state.transitions[input]


# This function will dump in a text file the current fsa.
# first line correspond to:
#
#  start_state final_state1 final_state2 ...
#
# Then every other line is just a tuple of edges.
def dump(fsa, filename):
    file = open(filename, "w")
    line = str(fsa.startState)
    for fs in fsa.finalStates:
        line += " " + str(fs)
    lines = [line + "\n"]
    for label, state in fsa.states.items():
        for input, dest in state.transitions.items():
            input, output = input.split("|")
            for d in dest:
                lines.append("%s %s %s %s\n" % (str(label), input, output, str(d)))
    file.writelines(lines)
    
# This function takes a list of symbol and will create
# a NFA which will recognize the sequence of those symbols.
# It means that for ["a", "b", "c"], this NFA will recognize
# the string sequence "abc".
def seq(symbols):
    edges = []
    for i in range(len(symbols)):
        edges.append((i, symbols[i], i + 1))
    return create(0, [len(symbols)], edges)
