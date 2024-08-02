# formal-modeling-of-structural-repetition


How to reproduce the results in the ISMIR paper:
1. Run the `ismirExperiment` function in the `src/IsmirExperiment.hs` file
2. The results will then be produced in `experiment/result` which are JSON files ready to be visualized by `d3.js` to produces figures in the paper. These files include:
    - `globalMetas.json` : Figure 7.
    - `pieceSizeComparison.json` : Figure 6.
    - `ruleStats.json` : Table 1. 
    - `sizeCurve` : Figure 5.

<!-- ## Execute

* Run `stack exec -- formal-modeling-of-structural-repetition-exe` to see "We're inside the application!"
* With `stack exec -- formal-modeling-of-structural-repetition-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test` -->
