module ProjectEuler.CommandLine.CmdGood
  ( cmdGood
  ) where

{-
  `pet good <problem id>` marks the solution to that problem as
  Solved, and record its output to answers.yaml

  TOOD: not implemented yet.
 -}

{-
  Implementation draft:

  - for marking a problem as solved:
    we just need something quick and simple, I'll
    going to assume that all solutions are formatted in the following way:

    > problem :: Problem
    > <... some contents (maybe several lines, but must be non-empty ...>
    > <end of file or an empty line>

    Also the string "Unsolved" is present exactly once in this section.

    With this assumption the update should be straightforward to do
    using following steps:

    + find the line that equals "problem :: Problem" (trailing spaces allowed)
    + keep consuming lines until hitting end of file or an empty line
      (spaces are ignored)
    + in the consumed section, there should be exactly one occurrence of string "Unsolved",
      change that into "Solved", and write other content back without change.

  - for recording the current solution: execute, then update data/answers.yaml
    for this to work we'll need `updateEditZone` from CmdSync to be exported
    and extend that function to allow modifying base on original contents.
 -}

cmdGood :: [String] -> IO ()
cmdGood _ = pure ()
