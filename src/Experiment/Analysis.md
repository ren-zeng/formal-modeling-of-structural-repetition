# Patterns report

- What are the patterns of a corpus?
  > e.g. "ii-V-I", "12-bar blues form" ...
  > In a data-driven way guided by efficient compression of parse trees.
  - Syntactic definition (as composition of smaller patterns)
    - visualized as computation with holes.
  - Semantics (as a production rule)
    - visualized as `x -> f1(x) f2(x) ...`
  - Basic statistics 
    - size when expanded (number of rules involved)
    - number of terminals and non-terminals generated
  

- Where are the patterns in the corpus?
  > e.g. "pattern_1 occurs in piece_1...n"
  > e.g. "the most prevalent patterns are ..."
  - Usage (set of pairs (pattern, piece))
    - visualized with a heat map
      - x-axis: pattern sorted by corpus freq (decreasing)
      - y-axis: piece
  > but maybe not all high-frequency patterns are interesting, because some might be too trivial like "V_I". But these are still important.
  - Scatter plot on all patterns:
    - x-axis: number symbols generated
    - y-axis: frequency in corpus (each piece counted once)
  
- What is the dependency among patterns?
  > e.g. "A {continuation} in a {sentence} typically begin with a {sequence}"
  > e.g. {sequence} → {continuation} → {sentence}
