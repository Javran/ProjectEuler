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
- (TODO) Runs all solutions and generate a report regarding correctness and time elapsed for them.

`pet-example` is a simple script just to save some typing time.
To use them, edit its environment variables as appropriate and
rename droping the `-example` suffix.

## petbox

Project home: https://github.com/Javran/petbox

Stands for "ProjectEuler Toolbox".

Some commonly used functions are extracted as a separated package.
Feel free to use them & contribution welcomed.

(TODO) Note that there are still some frequently used functions
that I haven't moved to a common module - I'm still need to give it
some thoughts about how to have a separated module while easily
update it to support my own needs.

## Some more ideas about the "workspace" bit

- I want to have one giant binary that has everything, so we can:

    + share common code among different problems
    + note that every binary ships with RTS, having everything compiled
      together will save some space & linking time, with a slight cost
      (dynamic dispatching to one particular problem) at runtime.

- Also note that the data does not need to be there - we can have a caching
  mechanism that fetches data on the fly and put it locally for future uses.

- `ProjectEuler.GetData` works fine except when a new data file is added,
  we need to somehow force this module to recompile when it is the case.
