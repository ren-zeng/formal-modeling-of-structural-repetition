# [DONE] Rule distribution (bar plot)
   1. show also abstracted rules
      1. Prolongational
         1. Repeat
         2. Relative key
         3. Oct prol
         4. Hex prol
      2. Dominantic
         1. V-I
         2. tritone-I
         3. backdoor-I
         4. V(Oct) - I
      3. Predominantic
         1. Diatonic descending 5th
      4. Plagal
   2. stack the bars (instead of two plots)
      1. y axis - abstract rule category
      2. x axis - frequency
   3. plot rule distribution againt tree depth where they occur (0-1) 
      1. "1" represents max depth of a tree
      2. stacked histogram against depth (0-1) (each large bar has the same height and width but different partitions)
   
# [DONE] compression scatter plot 
1. add projected distribution on both axes
2. the distribution of compression rate in a seperate plot

# [DONE] pattern frequency-size scatter plot
1. each point add a number represents the average pattern occurance depth (if A contains B, only consider A)


TODO: 

# [Done] pattern dependency plot 
1. group patterns based on their imports so that the cables are more managed.
2. for each pattern, show the average depth (0-1) by a bar pointed outward.

# [Done] pattern citation "impact" (entropy, freq)
- rank pattern `p` based summing (freq,rank) of the dependents of `p` 
    - Goal: increase with freq and entropy of citation (linear or quadratic)
    - Solution: 
    - moment of inertia (with/without the square) 
      - `factor of y on x = rank on (occurrence with x) * (occurrence with x)`
      - `impact(x) = sum_{y directly cite x}(factor of y on x)`


# scatter plot (freq vs size)
- maybe normalize over the frequency of all size k patterns
- or inform about the ceiling 
- null hypothesis: just size matters for frequency `p(freq|size) has low variance`
- null hypothesis 2: repeats are just random
  - preserve distribution of rules
  - 
- hope to find: `p(freq|size)` large variance

# Null corpus (for size vs freq metric) {the model can charaterize what make human composed music special (focusing on how patterns are reinforces, deliberate choices not modeled by randomly combining smaller patterns)}
- replace elaboration on X by another elaboration on X in other occurrances
  - (easy baseline) [non-syntactic] node rewrite {pattern is not just about the topology of tree fragment}
    - preserving distribution of rules
    - preserving shape
    - choose another production rule as node (compatible arity)
  - (hard baseline) [syntactic] subtree exchange 
    - preserving distribution of rules
    - preserving total size of corpus
    - for each node toss a coin to decide swap or not
      - if swap, sample a tree from the corpus with the same NT and exchange the two occurances
  
# framing 
1. "schemata are meaningful because we can find them" but we found many patterns that are not as interesting. Which means there are other important factor. 
   1. occurances is not a sufficient criteria for musically meaningful patterns
2. for a given depth, pattern occurs more on real corpus than baseline
3. the emergence of patterns cannot be simply described as {...} but things like {..} also matters. 
   - null hypothesis: 
   - we can show that real corpus does not conform to this hypothesis which means that there is a deliberate choice by composer when choosing patterns.