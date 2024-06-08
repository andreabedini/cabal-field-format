{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | A parse result type for parsers from AST to Haskell types.
module Distribution.Fields.ParseResult
  ( ParseResult,
    runParseResult,
    recoverWith,
    parseWarning,
    parseWarnings,
    parseFailure,
    parseFatalFailure,
    parseFatalFailure',
    withoutWarnings,
  )
where

import Data.List.NonEmpty
import Distribution.Parsec.Error (PError (..))
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..), PWarning (..))

-- | A monad with failure and accumulating errors and warnings.
newtype ParseResult a = PR
  { unPR ::
      forall r.
      PRState ->
      (PRState -> r) -> -- failure, but we were able to recover a new-style spec-version declaration
      (PRState -> a -> r) -> -- success
      r
  }

-- Note: we have version here, as we could get any version.
data PRState = PRState ![PWarning] ![PError]

emptyPRState :: PRState
emptyPRState = PRState [] []

-- | Forget 'ParseResult's warnings.
--
-- @since 3.4.0.0
withoutWarnings :: ParseResult a -> ParseResult a
withoutWarnings m = PR $ \s failure success ->
  unPR m s failure $ \ !s1 -> success (s1 `withWarningsOf` s)
  where
    withWarningsOf (PRState _ e) (PRState w _) = PRState w e

-- | Destruct a 'ParseResult' into the emitted warnings and either
-- a successful value or
-- list of errors and possibly recovered a spec-version declaration.
runParseResult :: ParseResult a -> ([PWarning], Either (NonEmpty PError) a)
runParseResult pr = unPR pr emptyPRState failure success
  where
    failure (PRState warns []) = (warns, Left (PError zeroPos "panic" :| []))
    failure (PRState warns (err : errs)) = (warns, Left (err :| errs))

    success (PRState warns []) x = (warns, Right x)
    -- If there are any errors, don't return the result
    success (PRState warns (err : errs)) _ = (warns, Left (err :| errs))

instance Functor ParseResult where
  fmap f (PR pr) = PR $ \ !s failure success ->
    pr s failure $ \ !s' a ->
      success s' (f a)
  {-# INLINE fmap #-}

instance Applicative ParseResult where
  pure x = PR $ \ !s _ success -> success s x
  {-# INLINE pure #-}

  f <*> x = PR $ \ !s0 failure success ->
    unPR f s0 failure $ \ !s1 f' ->
      unPR x s1 failure $ \ !s2 x' ->
        success s2 (f' x')
  {-# INLINE (<*>) #-}

  x *> y = PR $ \ !s0 failure success ->
    unPR x s0 failure $ \ !s1 _ ->
      unPR y s1 failure success
  {-# INLINE (*>) #-}

  x <* y = PR $ \ !s0 failure success ->
    unPR x s0 failure $ \ !s1 x' ->
      unPR y s1 failure $ \ !s2 _ ->
        success s2 x'
  {-# INLINE (<*) #-}

#if MIN_VERSION_base(4,10,0)
  liftA2 f x y = PR $ \ !s0 failure success ->
      unPR x s0 failure $ \ !s1 x' ->
      unPR y s1 failure $ \ !s2 y' ->
      success s2 (f x' y')
  {-# INLINE liftA2 #-}
#endif

instance Monad ParseResult where
  return = pure
  (>>) = (*>)

  m >>= k = PR $ \ !s failure success ->
    unPR m s failure $ \ !s' a ->
      unPR (k a) s' failure success
  {-# INLINE (>>=) #-}

-- | "Recover" the parse result, so we can proceed parsing.
-- 'runParseResult' will still result in 'Nothing', if there are recorded errors.
recoverWith :: ParseResult a -> a -> ParseResult a
recoverWith (PR pr) x = PR $ \ !s _failure success ->
  pr s (\ !s' -> success s' x) success

-- | Add a warning. This doesn't fail the parsing process.
parseWarning :: Position -> PWarnType -> String -> ParseResult ()
parseWarning pos t msg = PR $ \(PRState warns errs) _failure success ->
  success (PRState (PWarning t pos msg : warns) errs) ()

-- | Add multiple warnings at once.
parseWarnings :: [PWarning] -> ParseResult ()
parseWarnings newWarns = PR $ \(PRState warns errs) _failure success ->
  success (PRState (newWarns ++ warns) errs) ()

-- | Add an error, but not fail the parser yet.
--
-- For fatal failure use 'parseFatalFailure'
parseFailure :: Position -> String -> ParseResult ()
parseFailure pos msg = PR $ \(PRState warns errs) _failure success ->
  success (PRState warns (PError pos msg : errs)) ()

-- | Add an fatal error.
parseFatalFailure :: Position -> String -> ParseResult a
parseFatalFailure pos msg = PR $ \(PRState warns errs) failure _success ->
  failure (PRState warns (PError pos msg : errs))

-- | A 'mzero'.
parseFatalFailure' :: ParseResult a
parseFatalFailure' = PR pr
  where
    pr (PRState warns []) failure _success = failure (PRState warns [err])
    pr s failure _success = failure s

    err = PError zeroPos "Unknown fatal error"
