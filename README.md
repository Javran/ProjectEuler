# ProjectEuler

[![Build Status](https://travis-ci.org/Javran/ProjectEuler.svg?branch=master)](https://travis-ci.org/Javran/ProjectEuler)

My Project Euler solutions written in haskell

Please first try to solve problems independently before seeking for help.

## petbox

Project home: https://github.com/Javran/petbox

Stands for "ProjectEuler Toolbox".

Some commonly used functions are extracted as a separated package.
Feel free to use them & contribution welcomed.

## Migration

Admittedly this repo is a bit aged, so I want to have it simplified
and eventually become a better workspace for solving ProjectEuler problems.

Some ideas:

- I want to have one giant binary that has everything, so we can:

    + share common code among different problems
    + note that every binary ships with RTS, having everything compiled
      together will save some space & linking time, with a slight cost
      (dynamic dispatching to one particular problem) at runtime.

- Rethink about using Shake - the building process is already handled well
  by stack, all we need is just a way to run specific programs and
  collect some statistics for it. And we should be able to do this just within Haskell.

- Boilerplate setup - obviously we don't want to:

  (1) Create a module file with a skeleton code
  (2) Import it somewhere for Main to find
  (3) Update package.yaml so that the module is known to cabal or stack

  I haven't give it too much thought about how should we improve this though.

- Also note that the data does not need to be there - we can have a caching
  mechanism that fetches data on the fly and put it locally for future uses.
