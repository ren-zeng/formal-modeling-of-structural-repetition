# Rule distribution (bar plot)

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

# compression scatter plot 
1. add projected distribution on both axes
2. the distribution of compression rate in a seperate plot
3. each point add a number represents the average pattern occurance depth (if A contains B, only consider A)

TODO: 
# pattern frequency-size scatter plot


# plot rule distribution againt tree depth where they occur (0-1) 
1. "1" represents max depth of a tree
2. stacked histogram against depth (0-1) (each large bar has the same height and width but different partitions)

# pattern dependency plot 
1. group patterns based on their imports so that the cables are more managed.
2. for each pattern, show the average depth (0-1) by a bar pointed outward.

# pattern citation "impact" (entropy, freq)
- rank pattern `p` based summing (freq,rank) of the dependents of `p` 
    - Goal: increase with freq and entropy of citation (linear or quadratic)
    - Solution: 
    - moment of inertia (with/without the square) 
      - `factor of y on x = rank on (occurrence with x) * (occurrence with x)`
      - `impact(x) = sum_{y directly cite x}(factor of y on x)`