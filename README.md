# Javran's ProjectEuler Workspace

[![Build Status](https://travis-ci.org/Javran/ProjectEuler.svg?branch=master)](https://travis-ci.org/Javran/ProjectEuler)

My Project Euler solutions and some more stuff that
makes working with problems and keeping track of correctness & efficiency
a bit easier (the "workspace" bit).

- `src/ProjectEuler` is the place for both solutions and stuff
  commonly shared. In particular, `ProblemX.hs` where `X` is a number,
  is the program for the corresponding problem.

## Executables

`pet` is the binary handles everything:

- It creates new solution from template and update related parts of the project automatically.
- Solutions are all linked into this binary for execution.
- Runs all solutions and generate a report regarding correctness and time elapsed for them.

`pet-example` is a simple script just to save some typing time.
To use them, edit its environment variables as appropriate and
rename droping the `-example` suffix.

## petbox

Project home: https://github.com/Javran/petbox

Stands for "ProjectEuler Toolbox".

Some commonly used functions are extracted into this standalone package.
Feel free to use them & contribution welcomed.

## Some potential improvements

- Note that the data does not need to be there - we can have a caching
  mechanism that fetches data on the fly and put it locally for future uses.

- Now I start to find it being a bit wasteful to have a central place
  for all data files, as any change to them recompiles everything.
  I'm thinking about having a different template for problems
  that requires external data and have TemplateHaskell enabled for them by default.

- Investigate whether https://hackage.haskell.org/package/combinat could be useful to include.
