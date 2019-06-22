# Javran's ProjectEuler Workspace

This directory is supposed to be the new project home for the migration.

## Directories

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

## Migration

Current in the process of migrating old codes.

For now the plan is to migrate "slow" solutions first.
By "slow" I meant those that takes more than one second to complete.
The idea is that those problems might either have more room for improvement,
or involve more complexity to surface problems related to this migration.

Listing (TODO):

- 35
- 36
- 38
- 43
- 44
- 47
- 60
- 70
- 73
- 74
- 84
- 87
- 88
- 92
- 104
- 206
