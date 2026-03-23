# Formal Modeling of Structural Repetition using Tree Compression

## Set-up

### Setting up Haskell environment

Download and install `ghcup` following the link from <https://www.haskell.org/ghcup/>. It provides a terminal user interface (invoked by `ghcup tui`) to configure haskell environments.
Use the recommended `GHC` version, and choose to install `cabal` and `HLS`.

### How to reproduce the results

Use `cabal run` to run all the experiments in the paper.

The datasets used in the article can be found at `experiment/DataSet`.

The experiment results are stored at `experiment/Result`.

The top-level script for the experiments can be found at `src/Experiment`

## Directory structure

### Tree compression

`src/Compression` contains the main logic for tree compression, which use a data structure named `SLFP` to compactly represent a forest of abstract syntax tree. The function `compressG` applies a single step  transformation on `SLFP`. The compression then defined as a fixed point of `compressG`. The helper function `compressGSteps` returns a step-by-step compression results (the last element of the list is the final result).

### Grammar

`src/Grammar` contains the grammar for different musical domains (jazz harmony and classical rhythm)

### Visualization

`src/VisualHTML` contains all the plotting scripts. The top level function in `src/VisualHTML/MakeHTML` essentially works by inlining the data to visualized to a fixed HTML template (uses D3.js for interactive plot).
