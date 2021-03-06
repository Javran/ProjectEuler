{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , UndecidableInstances
  , FlexibleInstances
  #-}
module ProjectEuler.Types
  ( ProblemStatus(..)
  , Problem(..)
  , pureProblem
  , PEM
  , getPEM
  , runPEM
  , logT
  , logText
  , io
  , Rounded(..)
  ) where

import Control.Arrow
import Control.Monad.Base
import Control.Monad.Random
import Control.Monad.Trans.Control
import Control.Monad.Writer.Strict -- TODO: use CPS
import Text.Printf
import TextShow

import qualified Data.DList as DL
import qualified Data.Text as T

data ProblemStatus
  = {-
      A problem marked as solved is a problem that:

      (1) gets the correct answer
      (2) execution time is reasonable (i.e. "one minute rule")

      And all solved problems should have test coverage.
     -}
    Solved
  | {-
      A problem marked as unsolved are all the problems that doesn't qualify
      to be marked as solved. This will include solutions that give
      correct answer but its execution time is not acceptable.
     -}
    Unsolved
  deriving (Eq)

{-
  problemRun performs some arbitrary action,
  and perhaps give some outputs through stdout to
  help deriving final answers to the problem.

  It it also intentional that this action does not
  take any arguments - if we want to setup tests
  or run it against some dynamic inputs,
  we can just export module internals.
 -}
data Problem
  = Problem
  { problemId :: Int
  , problemStatus :: ProblemStatus
  , problemRun :: PEM ()
  }

pureProblem :: TextShow r => Int -> ProblemStatus -> r -> Problem
pureProblem pId pSt result =
  Problem pId pSt (logT result)

io :: IO a -> PEM a
io = liftIO

logT :: TextShow v => v -> PEM ()
logT v = tell (DL.singleton (showt v))

logText :: T.Text -> PEM ()
logText v = tell (DL.singleton v)

runPEM :: PEM a -> IO (a, [T.Text])
runPEM (PEM comp) = second DL.toList <$> runWriterT comp

{-
  PEM is short for ProjectEuler Monad, which allows IO actions
  and support logging with Text.

  The motivation is that we want to check the output to make sure it's correct
  and capturing stdout / stderr isn't the preferable way to go.

  Note that it's tricky to handle IO exceptions for monad transformers,
  so it's preferred to use lifted-base when we do need exception-handling.
 -}
newtype PEM a
  = PEM { getPEM :: WriterT (DL.DList T.Text) IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter (DL.DList T.Text)
    , MonadIO
    , MonadBase IO
    , MonadBaseControl IO
    , MonadFail
    , MonadRandom
    )

{-
  Rounded defines a TextShow instance
  For floating number outputs that require rounding to a specific precision.
 -}
data Rounded f = Rounded Int f

instance TextShow (Rounded Double) where
  showb (Rounded p v) = fromString (printf "%.*f" p v)
