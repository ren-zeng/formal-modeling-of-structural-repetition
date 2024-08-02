# Formal Modeling of Structural Repetition using Tree Compression


## How to reproduce the results in the ISMIR paper
1. Run the `ismirExperiment` function in the `src/IsmirExperiment.hs` file
2. The results will then be produced in `experiment/result` which are JSON files ready to be visualized by `d3.js` to produces figures in the paper. These files include:
    - `globalMetas.json` : Figure 7.
    - `pieceSizeComparison.json` : Figure 6.
    - `ruleStats.json` : Table 1.
    - `sizeCurve` : Figure 5.

## Preprocessing
The preprocessed rule-labled trees can be found at `experiment/data/ProofTrees`. 
The corpus data can be found at `experiment/data/treebank.json`, which are forked from [JazzHarmonyTreebank](https://github.com/DCMLab/JazzHarmonyTreebank).
<!-- ## Execute

* Run `stack exec -- formal-modeling-of-structural-repetition-exe` to see "We're inside the application!"
* With `stack exec -- formal-modeling-of-structural-repetition-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test` -->
