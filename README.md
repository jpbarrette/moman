# Moman
## Description
This is will eventually be a suite of tools to be used by an orthographic/grammatical checker and the checker itself. The tools are currently coded in Python, but I began to rewrite it in Lisp. Moman, the suite itself, will be consisted of the following tools:
* *FineNight* is the FSA library.
* A FST library. (Not yet implemented)
* *ZSpell* is the orthographic checker.

## FineNight
The *FineNight* library contains many algorithms for Finite State Automatons. That includes:
* Union of two FSAs
* Intersection of two FSAs
* Complement of a FSAs
* Difference of two FSAs
* Reversal of a FSA
* Closure of a FSA
* Concatenation of two FSAs
* Determination of a NFA
* Equivalence test
* Minimization algorithm
* Construction of an IADFA from a sorted dictionary
* Graphviz support
* Error-Tolerant IADFA

Almost all algorithms were taken from the book [Introduction to Automata Theory, Languages, and Computation](#hopcroft01). The minimization algorithm is an implementation of Brzozowski's method [<a href="#brzozowski">2</a>]. In this method, the (possibly non-deterministic) automaton is reversed, determinized, reversed and determinized. I'll eventually add the Hopcroft's nlog(n) minimization algorithm [<a href="#hopcroft">3</a>]

<a name="hopcroft01"/>[John E. Hopcroft](http://www.cs.cornell.edu/Info/Department/Annual95/Faculty/Hopcroft.html), <a href="http://theory.stanford.edu/~rajeev/">Rajeev Motwani</a> and
	      <a href="http://www-db.stanford.edu/~ullman/">Jefferey D. Ullman</a>, <a href="http://www-db.stanford.edu/~ullman/ialc.html">Introduction to Automata Theory, Languages and Computation</a>, 2nd edition, Adison-Wesley, 2001.
