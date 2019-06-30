# Javran's ProjectEuler Workspace

[![Build Status](https://travis-ci.org/Javran/ProjectEuler.svg?branch=master)](https://travis-ci.org/Javran/ProjectEuler)

My Project Euler solutions and some more stuff that
makes working with problems and keeping track of correctness & efficiency
a bit easier (the "workspace" bit).

- `src/ProjectEuler` is the place for both solutions and stuff
  commonly shared. In particular, `ProblemX.hs` where `X` is a number,
  is the program for the corresponding problem.

## Executables

- `pet`: main program.
  `pet run <problem-number>` to run the program for the specific problem.

- `templater`: scripting for automating module list update.
  (TODO) It is also in plan to support creating new problem program.

- `XXX-example`: these are simple scripts just to save some typing time.
  To use them, edit its environment variables as appropriate and
  rename droping the `-example` suffix.

## petbox

Project home: https://github.com/Javran/petbox

Stands for "ProjectEuler Toolbox".

Some commonly used functions are extracted as a separated package.
Feel free to use them & contribution welcomed.

## Migration

Current state: most of the ancient solutions have been migrated.

But there are 5 problems that I do have answered
without collecting the problem, will add them retroactively:

- 66
- 90
- 91
- 93
- 99

## Some more ideas about the "workspace" bit

- I want to have one giant binary that has everything, so we can:

    + share common code among different problems
    + note that every binary ships with RTS, having everything compiled
      together will save some space & linking time, with a slight cost
      (dynamic dispatching to one particular problem) at runtime.

- Also note that the data does not need to be there - we can have a caching
  mechanism that fetches data on the fly and put it locally for future uses.
