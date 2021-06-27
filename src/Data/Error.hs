{-# LANGUAGE OverloadedStrings #-}
module Data.Error
  ( Error,
    newError,
    addContext,
    prettyError,
    unwrapError,
    expectError,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text

-- | The canonical @Error@ type.
--
-- It can be
--
-- * created from a human-readable error message ('newError')
-- * more semantic context can be added to an existing @Error@ ('addContext')
-- * pretty-printed (`prettyError`)
newtype Error = Error [Text]

-- | Create an ad-hoc 'Error' from an error message.
newError :: Text -> Error
newError msg = Error [msg]

-- | Add a higher-level context to an 'Error'.
--
-- For example, your code hits a “file not found” I/O exception.
-- Instead of propagating it unseen, you catch it and annotate it with 'addContext',
-- and describe why you wanted to open the file in the first place:
--
-- @
-- addContext "Trying to open config file"
--   $ newError "file not found: ./foo"
-- @
--
-- This way, when a user see the error, they will understand better what happened:
--
-- @
-- "Trying to open config file: file not found: ./foo"
-- @
--
-- See 'prettyError'.
addContext :: Text -> Error -> Error
addContext e (Error es) = Error $ e : es

-- | Pretty print the error.
--
-- It will print all context messages, starting with the outermost.
--
-- Example:
--
-- >>> prettyError $ newError "file not found: ./foo"
-- "file not found: ./foo"
--
-- >>> :{
--   prettyError
--     $ addContext "Trying to open config file"
--       $ newError "file not found: ./foo"
-- :}
-- "Trying to open config file: file not found: ./foo"
prettyError :: Error -> Text
prettyError (Error es) = Text.intercalate ": " es

-- | Return the value from a potentially failing computation.
--
-- Abort with the 'Error's message if it was a 'Left'.
--
-- Panic: if Error
--
-- Example:
--
-- >>> unwrapError $ Left (newError "oh no!")
-- *** Exception: oh no!
--
-- >>> unwrapError $ Right 42
-- 42
unwrapError :: Either Error p -> p
unwrapError e = case e of
  Left err -> error (prettyError err & Text.unpack)
  Right a -> a

-- | Return the value from a potentially failing computation.
--
-- Abort with the error message if it was an error.
--
-- The text message is added to the 'Error' as additional context before aborting.
--
-- Panic: if Error
--
-- Example:
-- >>> expectError "something bad happened" $ Left (newError "oh no!")
-- *** Exception: something bad happened: oh no!
--
-- >>> expectError "something bad happened" $ Right 42
-- 42
expectError :: Text -> Either Error p -> p
expectError context e = case e of
  Left err ->
    error
      ( err
          & addContext context
          & prettyError
          & Text.unpack
      )
  Right a -> a
