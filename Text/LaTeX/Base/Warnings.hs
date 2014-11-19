
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functionality for check a 'LaTeX' value for
--   possibly undesired things (like the call to an undefined label),
--   returning 'Warning's. These are called 'Warning's because they
--   never terminate the program execution.
module Text.LaTeX.Base.Warnings (
   -- * Warnings datatype
   Warning (..)
 , TeXCheck
 , check
 , checkFromFunction
   -- * Several checkings
 , checkLabels
 , checkClass
 , checkDoc
   -- * Complete checking
 , checkAll
 ) where

import Text.LaTeX.Base.Syntax
import Control.Monad.Trans.State
import Data.Text
import Data.Maybe
import Control.Arrow
import Data.Monoid
import Control.Monad

-- | List of possible warnings.
data Warning =
   UnusedLabel Text    -- ^ There is an unused label. Argument is its name.
 | UndefinedLabel Text -- ^ There is a reference to an undefined label. Arguments is the name.
   --
 | NoClassSelected     -- ^ No class selected with 'documentclass'.
 | NoDocumentInserted  -- ^ No 'document' inserted.
   --
 | CustomWarning Text  -- ^ Custom warning for custom checkings. Use it as you want.
   deriving (Eq,Show)

-- | A 'TeXCheck' is a function that checks possible warnings from a 'LaTeX' value.
--   Use the 'Monoid' instance to combine check functions.
newtype TeXCheck = TC { check :: LaTeX -> [Warning] -- ^ Apply a checking.
                      }
-- | Build a 'TeXCheck' from a function.
checkFromFunction :: (LaTeX -> [Warning]) -> TeXCheck
checkFromFunction = TC

instance Monoid TeXCheck where
 mempty = TC $ const []
 mappend (TC tc1) (TC tc2) = TC $ uncurry mappend . (tc1 &&& tc2)

-- | Check with 'checkLabels', 'checkClass' and 'checkDoc'.
checkAll :: TeXCheck
checkAll = mconcat [ checkLabels , checkClass , checkDoc ]

-- Searching for 'documentclass' and 'document'

type BoolSt = State Bool

-- | Check if a document class is specified for the document (using 'documentclass').
checkClass :: TeXCheck
checkClass = TC $ \l -> if execState (classcheck l) False then [] else [NoClassSelected]

classcheck :: LaTeX -> BoolSt ()
classcheck (LaTeX bs) = mapM_ go bs where
  go (TeXComm c _) = when (c == "documentclass") (put True)
  go (TeXBraces l) = classcheck l
  go _ = return ()

-- | Check if the 'document' environment is called in a 'LaTeX'.
checkDoc :: TeXCheck
checkDoc = TC $ \l -> if execState (doccheck l) False then [] else [NoDocumentInserted]

doccheck :: LaTeX -> BoolSt ()
doccheck (LaTeX bs) = mapM_ go bs where
  go (TeXEnv n _ _) = when (n == "document") $ put True
  go (TeXBraces l)  = doccheck l
  go _              = return ()

-- Checking labels

data LabWarn =
   RefNoLabel Text
 | LabelNoRef Text
 | LabelRef Text

labWarnToWarning :: LabWarn -> Maybe Warning
labWarnToWarning (RefNoLabel n) = Just $ UndefinedLabel n
labWarnToWarning (LabelNoRef n) = Just $ UnusedLabel n
labWarnToWarning _ = Nothing

type LabSt = State [LabWarn]

-- | Checking for unused labels or references tu undefined labels.
checkLabels :: TeXCheck
checkLabels = TC $ \l -> catMaybes . fmap labWarnToWarning $ execState (labcheck l) []

labcheck :: LaTeX -> LabSt ()
labcheck (LaTeX bs) = mapM_ go bs where
  go (TeXComm c [FixArg (LaTeX [TeXRaw n])]) =
    case c of
      "label"   -> newlab n
      "ref"     -> newref n
      "pageref" -> newref n
      _ -> return ()
  go (TeXEnv _ _ l) = labcheck l
  go (TeXMath _ l) = labcheck l
  go (TeXBraces l) = labcheck l
  go _ = return ()

newlab :: Text -> LabSt ()
newlab t = do
 st <- get
 let addLab :: Text -> [LabWarn] -> [LabWarn]
     addLab n [] = [LabelNoRef n]
     addLab n l@(x:xs) = let ys = x : addLab n xs in
       case x of
        RefNoLabel m -> if n == m then LabelRef n : xs
                                  else ys
        LabelNoRef m -> if n == m then l
                                  else ys
        LabelRef   m -> if n == m then l
                                  else ys
 put $ addLab t st

newref :: Text -> LabSt ()
newref t = do
 st <- get
 let addRef :: Text -> [LabWarn] -> [LabWarn]
     addRef n [] = [RefNoLabel n]
     addRef n l@(x:xs) = let ys = x : addRef n xs in
       case x of
        RefNoLabel m -> if n == m then l
                                  else ys
        LabelNoRef m -> if n == m then LabelRef n : xs
                                  else ys
        LabelRef   m -> if n == m then l
                                  else ys
 put $ addRef t st

