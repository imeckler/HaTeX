
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Definition of the 'LaTeXC' class, used to combine the classic applicative and
--   the latter monadic interfaces of /HaTeX 3/. The user can define new instances
--   as well, adding flexibility to the way /HaTeX/ is used.
module Text.LaTeX.Base.Class (
   LaTeXC (..)
 , Monoid (..)
   -- * Combinators
   -- ** From @LaTeX@
 , fromLaTeX
   -- ** Lifting
   -- | Lifting functions from 'LaTeX' functions to functions over any instance of 'LaTeXC'.
   --   In general, the implementation is as follows:
   --
   -- > liftLN f x1 ... xN = liftListL (\[x1,...,xN] -> f x1 ... xN) [x1,...,xN]
   --
 , liftL
 , liftL2
 , liftL3
   -- ** Others
 , comm0
 , comm1
 , commS
 , braces
 ) where

import Text.LaTeX.Base.Syntax
import Data.Monoid
import Data.String

-- TODO: The purpose of this module is unclear to me, so I'm not really
-- sure how to modify it for the new type.

-- | This is the class of 'LaTeX' code generators. It has 'Monoid' and 'IsString' as
--   superclasses.
class (Monoid l,IsString l) => LaTeXC l where
 -- | This method must take a function that combines a list of 'LaTeX' values into a new one,
 --   and creates a function that combines @l@-typed values. The combining function can be
 --   seen as a function with 0 or more 'LaTeX' arguments with a 'LaTeX' value as output.
 liftListL :: ([LaTeX] -> LaTeX) -> [l] -> l

-- | This instance just sets @liftListL = id@.
instance LaTeXC LaTeX where
 liftListL = id

-- COMBINATORS

-- | Map a 'LaTeX' value to its equivalent in any 'LaTeXC' instance.
fromLaTeX :: LaTeXC l => LaTeX -> l
fromLaTeX l = liftListL (\_ -> l) []

-- | Lift a inner function of 'LaTeX' values into any 'LaTeXC' instance.
liftL :: LaTeXC l => (LaTeX -> LaTeX) -> l -> l
liftL f x = liftListL (\[x] -> f x) [x]

-- | Variant of 'liftL' with a two arguments function.
liftL2 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX) -> l -> l -> l
liftL2 f x y = liftListL (\[x,y] -> f x y) [x,y]

-- | Variant of 'liftL' with a three arguments function.
liftL3 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l
liftL3 f x y z = liftListL (\[x,y,z] -> f x y z) [x,y,z]

-- | A simple (without arguments) and handy command generator
--   using the name of the command.
--
-- > comm0 str = fromLaTeX $ TeXComm str []
--
comm0 :: LaTeXC l => String -> l
comm0 str = fromLaTeX . LaTeX . return $ TeXComm str []

-- | A one parameter command generator using the name of the command.
--   The parameter will be rendered as a fixed argument.
--
-- > comm1 str = liftL $ \l -> TeXComm str [FixArg l]
--
comm1 :: LaTeXC l => String -> l -> l
comm1 str = liftL $ \l -> LaTeX . return $ TeXComm str [FixArg l]

-- | Like 'comm0' but using 'TeXCommS', i.e. no \"{}\" will be inserted to protect
-- the command's end.
--
-- > commS = fromLaTeX . TeXCommS
--
commS :: LaTeXC l => String -> l
commS = fromLaTeX . LaTeX . return . TeXCommS

-- | A lifted version of the 'TeXBraces' constructor.
--
-- > braces = liftL TeXBraces
--
braces :: LaTeXC l => l -> l
braces = liftL (LaTeX . return . TeXBraces)
