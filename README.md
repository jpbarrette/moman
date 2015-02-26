# Moman
## Description
This was supposed to be a suite of tools to be used by an orthographic/grammatical checker and the checker itself. However, the project is mainly dead right now. But I encourage you to look through the code and use it as inspiration/reference. The tools are currently coded in Python, but I started a while back to rewrite it in Lisp (which will never be finished). Moman, the suite itself, consist of the following tools:

* [FineNight](#finenight) is the FSA library.
* A FST library. (Not yet implemented)
* [ZSpell](#zspell) is the orthographic checker.

Mostly, the only part of the tools suite which is worthwhile mentioning is the "Fast String Correction" which is used by [Lucene's](https://lucene.apache.org/) FuzzyQuery. You can read about the inclusion of this project in Lucene by reading Michael McCandless's [article](http://blog.mikemccandless.com/2011/03/lucenes-fuzzyquery-is-100-times-faster.html).

## FineNight
<a name="finenight"/>The *FineNight* library contains many algorithms for Finite State Automatons. That includes:
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
* Error-Tolerant IADFA (starred in Michael McCandless's Mike MChttp://blog.mikemccandless.com/2011/03/lucenes-fuzzyquery-is-100-times-faster.html

Almost all algorithms were taken from the book [Introduction to Automata Theory, Languages, and Computation](#hopcroft01). The minimization algorithm is an implementation of [Brzozowski's method](#brzozowski). In this method, the (possibly non-deterministic) automaton is reversed, determinized, reversed and determinized. I'll eventually add the [Hopcroft's nlog(n) minimization algorithm](#hopcroft).

## ZSpell
<a name="zspell"/><i>ZSpell</i> is meant to be a concurrent of <a href="http://aspell.sourceforge.net/">aspell</a>, made by Kevin Atkinson. At this time, <i>ZSpell</i> can suggest words with a Levenshtein-distance of one. Before we were using [Kemal Oflazer's algorithm](#oflazer96errortolerant). This algorithm is very slow, but now we use a faster algorithm ([Schulz's and Mihov's algorithm](#schulz02fast)). However, only substitution, removal and insertion are used for the faster algorithm. It means that transpositions errors, like "ehllo" -> "hello", are considered as two operations. 

TODOs includes:
* Add transposition errors for Levenshtein-distance algorithm.
* Add phonetic errors (spelling by sound).
* Add derivation errors.

## References
* <a name="hopcroft01"/>[John E. Hopcroft](http://www.cs.cornell.edu/Info/Department/Annual95/Faculty/Hopcroft.html), <a href="http://theory.stanford.edu/~rajeev/">Rajeev Motwani</a> and <a href="http://www-db.stanford.edu/~ullman/">Jefferey D. Ullman</a>, <i><a href="http://www-db.stanford.edu/~ullman/ialc.html">Introduction to Automata Theory, Languages and Computation</a></i>, 2nd edition, Adison-Wesley, 2001.
* <a name="brzozowski"/><a href="http://maveric.uwaterloo.ca/~brzozo/">J. A. Brzozowski</a>,
	      <i>Canonical regular expressions and minimal state graphs for definite events</i>, 
	      in Mathematical Theory of Automata, Volume 12 of MRI Symposia Series, 
	      pp. 529-561,      Polytechnic Press, Polytechnic Institute of Brooklyn, N.Y.,
	      1962.
* <a name="hopcroft"/> <a href="http://www.cs.cornell.edu/Info/Department/Annual95/Faculty/Hopcroft.html">
		John E. Hopcroft
	      </a>, 
	      <a href="http://historical.ncstrl.org/litesite-data/stan/CS-TR-71-190.pdf">
		<i>An n log n algorithm for minimizing the states in a finite automaton</i>
	      </a>, 
	      in The Theory of Machines and Computations, Z. Kohavi (ed.), pp. 189-196, 
	      Academic Press, 1971.
* <a name="oflazer96errortolerant"/>
	      <a href="http://www.nlp.cs.bilkent.edu.tr/~ko/">Kemal Oflazer</a>,
	      <a href="http://citeseer.ist.psu.edu/oflazer96errortolerant.html">
		<i>Error-tolerant Finite State Recognition with Applications to 
		  Morphological Analysis and Spelling Correction</i>
	      </a>,
	      Computational Linguistics, 22(1), pp. 73--89, March, 1996.
* <a name="schulz02fast"/>
	      <a href="http://www.cis.uni-muenchen.de/people/schulz.html">
		Klaus U. Schulz
	      </a> and 
	      <a href="http://lml.bas.bg/~stoyan/">Stoyan Mihov</a>,
		<a href="http://citeseer.ist.psu.edu/schulz02fast.html">
		  <i>Fast String Correction with Levenshtein-Automata</i>,
		</a>
		International Journal of Document Analysis and Recognition, 5(1):67--85, 2002.
* <a name="czech92optimal"/>
	      <a href="http://sun.iinf.polsl.gliwice.pl/~zjc/">
		Zbigniew J. Czech
	      </a>,
	      <a href="http://www.itee.uq.edu.au/~havas/">
		George Havas
	      </a> and 
	      Bohdan S. Majewski,
	      <a href="http://citeseer.ist.psu.edu/czech92optimal.html">
		<i>An Optimal Algorithm for Generating Minimal Perfect Hash Functions</i>
	      </a>, Information Processing Letters, 43(5):257--264, 1992.
