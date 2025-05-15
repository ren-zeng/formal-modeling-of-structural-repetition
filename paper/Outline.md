# Discoverying structural patterns in music using tree compression

While the ISMIR paper provides a introduction to a formal model together with a proof of concept implementation.
This article extends ISMIR paper by focusing on how it can be used to enrich our music understanding. In addition to the formalism, we present two systematic corpus studies on patterns in two different musical parameters, harmony and rhythm, in a data-driven way.

This article extends the ISMIR paper in two dimensions:

1. (music parameter): We run the model on two datasets with annotated trees.
      1. Jazz harmony tree bank  
      2. Rhythm

2. (music understanding): We use the model to answer the following questions about musical patterns:

For each question:
    1. what does it mean to answer this question?
    2. what type of evidence is needed to support the answer?

## Q1: What are the patterns of a corpus?
>
> Examples of human answer: "ii-V-I", "12-bar blues form", "sequences", "AABA", etc
> "ii-V-I is a succession of three chords where they fit diatonically as Roman numerals ii, V, I in some local key"
> "a sequence is a succession of segments with organized by the same logic (harmonic relations, etc)"
> "AABA is a pattern of repeat where a phrase is can be segmented into four parts in which part 1 2 and 4 are identical and part 3 is different.

 1. What does it mean to answer this question?
    1. Definition (in terms of what? It needs to support *identification* and *generalization*)
       1. Syntactic definition (as compositions of smaller patterns)  
          1. visualized as computation with holes.
       2. Semantics (as a production rule)
          1. visualized as `x -> f1(x) f2(x) ...`
          2. > ii-V-I: {C-7 -> D-7b5 G7 C-7, CM7 -> D-7 G7 CM7, ...}

## Q2: Where are the patterns?
>
> Examples of human answer:
  > Proofs of existence
    > piece occurrance: "pattern1 occurs in piece1...n in measure i-j"
    > frequency: "the most prevalent patterns are ..."

  > Interpretation
    > usage/function with some {context of interest}:
      > {Formal function} "The Barquinho is typically used as an opener" (Jazz chord progression)
      > {Formal function} "The Fonte is typically used as part of a transition" (Galant Schmeta)
      > {Formal function} "A cadential progression is used as end of a large section" (Classical chord progression)
      > {Formal+harmonic function} "A tritone substitution are typically used to elaborate the B section in a ABA form" (Jazz chord progression)
      > {Hypermeter}
      > depth in tree
- > In which piece does the pattern occur?  
  - Data = `Map Pattern (Set Piece)`
  - Presentation = `HeatMap`
    - x = Pattern (sorted by frequency)
    - y = Piece (sorted by name)
- > How are the patterns used in context (usage/function)?
  - > Location as tree location
    - Data = `TreeWithHighlight`
    - Presentation = `HighlightedTree`
      - pattern as highlighted tree fragment within tree for each piece.
  - > Location as time intervals (chord indicies)
    - Data = `List Segment` where `Segment = List (Int,Int)` e.g. [[chord 1-5,chord 7-9],[chord 12-16,chord 22-23]]
    - Presentation = TODO

  # Evidence

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
