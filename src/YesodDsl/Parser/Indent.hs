{-# LANGUAGE FlexibleContexts #-}
{-
Copyright (c)2010, Sam Anklesaria

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sam Anklesaria nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



-}
module YesodDsl.Parser.Indent (
    -- $doc
    
    -- * Types
    IndentParserT, runIndent,
    -- * Blocks
    withBlock, withBlock', block,
    -- * Indentation Checking
    indented, same, sameOrIndented, checkIndent, withPos,
    -- * Paired characters
    indentBrackets, indentAngles, indentBraces, indentParens,
    -- * Line Fold Chaining
    -- | Any chain using these combinators must used with 'withPos'
    (<+/>), (<-/>), (<*/>)
    ) where
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Token
import Control.Monad.State
import Control.Concatenative
import Debug.Trace

-- $doc
-- A module to construct indentation aware parsers. Many programming
-- language have indentation based syntax rules e.g. python and Haskell.
-- This module exports combinators to create such parsers. 
-- 
-- The input source can be thought of as a list of tokens. Abstractly
-- each token occurs at a line and a column and has a width. The column
-- number of a token measures is indentation. If t1 and t2 are two tokens
-- then we say that indentation of t1 is more than t2 if the column
-- number of occurrence of t1 is greater than that of t2.
-- 
-- Currently this module supports two kind of indentation based syntactic
-- structures which we now describe:
-- 
-- [Block] A block of indentation /c/ is a sequence of tokens with
-- indentation at least /c/.  Examples for a block is a where clause of
-- Haskell with no explicit braces.
-- 
-- [Line fold] A line fold starting at line /l/ and indentation /c/ is a
-- sequence of tokens that start at line /l/ and possibly continue to
-- subsequent lines as long as the indentation is greater than /c/. Such
-- a sequence of lines need to be /folded/ to a single line. An example
-- is MIME headers. Line folding based binding separation is used in
-- Haskell as well.

-- | Indentation sensitive parser type. Usually @ m @ will
--   be @ Identity @ as with any @ ParsecT @
type IndentParserT s u m a = ParsecT s u (StateT SourcePos m) a
    
-- | @ 'withBlock' f a b p @ parses @ a @ and optionally
--   @ b @ followed by an indented block of @ p @
--   , combining them with @ f @
withBlock :: (Stream s (StateT SourcePos m) Char, Monad m) => (a -> [b] -> c) ->
    IndentParserT s u m a -> IndentParserT s u m x -> IndentParserT s u m b -> IndentParserT s u m c
withBlock f a b p = withPos $ do
    r1 <- a
    r2 <- option [] (b >> indented >> block p)
    return (f r1 r2)

-- | Like 'withBlock', but throws away initial parse result
withBlock' :: (Stream s (StateT SourcePos m) Char, Monad m) =>
    IndentParserT s u m a -> IndentParserT s u m x -> IndentParserT s u m b -> IndentParserT s u m [b]
withBlock' = withBlock (flip const)

-- | Parses only when indented past the level of the reference
indented :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m ()
indented = do
    pos <- getPosition
    s <- get
    if biAp sourceColumn (<=) pos s then parserFail "not indented" else do
        put $ setSourceLine s (sourceLine pos)
        return ()

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m ()
sameOrIndented = same <|> indented

-- | Parses only on the same line as the reference
same :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m ()
same = do
    pos <- getPosition
    s <- get
    if biAp sourceLine (==) pos s then return () else parserFail "over one line"
    
-- | Parses a block of lines at the same indentation level
block :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m a -> IndentParserT s u m [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r

-- | Parses using the current location for indentation reference
withPos :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m a -> IndentParserT s u m a
withPos x = do
    a <- get
    p <- getPosition
    r <- put p >> x
    put a >> return r

-- | Ensures the current indentation level matches that of the reference
checkIndent :: (Stream s (StateT SourcePos m) Char, Monad m) => IndentParserT s u m ()
checkIndent = do
    s <- get
    p <- getPosition
    if biAp sourceColumn (==) p s then return () else parserFail "indentation doesn't match"

-- | Run the result of an indentation sensitive parse
runIndent :: Monad m => SourceName -> StateT SourcePos m a -> m a
runIndent s = flip evalStateT (initialPos s)

-- | '<+/>' is to indentation sensitive parsers what 'ap' is to monads
(<+/>) :: (Stream s (StateT SourcePos m) Char, Monad m) =>
    IndentParserT s u m (a -> b) -> IndentParserT s u m a -> IndentParserT s u m b
a <+/> b = ap a (sameOrIndented >> b)

-- | '<-/>' is like '<+/>', but doesn't apply the function to the parsed value
(<-/>) :: (Stream s (StateT SourcePos m) Char, Monad m) =>
    IndentParserT s u m a -> IndentParserT s u m b -> IndentParserT s u m a
a <-/> b = liftM2 const a (sameOrIndented >> b)

-- | Like '<+/>' but applies the second parser many times
(<*/>) :: (Stream s (StateT SourcePos m) Char, Monad m) =>
    IndentParserT s u m ([a] -> b) -> IndentParserT s u m a -> IndentParserT s u m b
a <*/> b = ap a (many (sameOrIndented >> b))

-- | parses with surrounding brackets
indentBrackets :: (Stream s (StateT SourcePos m) Char, Monad m) => GenTokenParser s u (StateT SourcePos m) -> IndentParserT s u m a -> IndentParserT s u m a
indentBrackets lexer p = withPos $ return id <-/> symbol lexer "[" <+/> p <-/> symbol lexer "]"

-- | parses with surrounding angle brackets
indentAngles :: (Stream s (StateT SourcePos m) Char, Monad m) => GenTokenParser s u (StateT SourcePos m) -> IndentParserT s u m a -> IndentParserT s u m a
indentAngles lexer p = withPos $ return id <-/> symbol lexer "<" <+/> p <-/> symbol lexer ">"

-- | parses with surrounding braces
indentBraces :: (Stream s (StateT SourcePos m) Char, Monad m) => GenTokenParser s u (StateT SourcePos m) -> IndentParserT s u m a -> IndentParserT s u m a
indentBraces lexer p = withPos $ return id <-/> symbol lexer "{" <+/> p <-/> symbol lexer "}"

-- | parses with surrounding parentheses 
indentParens :: (Stream s (StateT SourcePos m) Char, Monad m) => GenTokenParser s u (StateT SourcePos m) -> IndentParserT s u m a -> IndentParserT s u m a
indentParens lexer p = withPos $ return id <-/> symbol lexer "(" <+/> p <-/> symbol lexer ")"
