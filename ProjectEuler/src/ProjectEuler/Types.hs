{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , UndecidableInstances
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
  ) where

import Control.Arrow
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Writer
import TextShow

import qualified Data.DList as DL
import qualified Data.Text as T

data ProblemStatus = Solved | Unsolved

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
    )

