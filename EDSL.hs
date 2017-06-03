{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, KindSignatures, GADTs,
             ConstraintKinds #-}
module Text.Regex.EDSL
    -- * Data types
    ( Regex(..)
    , Dir(..)
    , Sign(..)
    , CharLit(..)
    , CharGroup(..)
    , GroupLit(..)
    -- * Helper functions
    , (<>)
    , charSpecial
    , wordBoundary
    , startOfLine
    , endOfLine
    , look
    , lookNot
    , charClass
    , classDiff
    , classInter
    , inv
    -- ** Character groups
    , range
    , groupSpecial
    , wordChar
    , digit
    ) where

import Data.Char
import Data.String (IsString(..))
import Data.Monoid ((<>))
import Numeric (showIntAtBase)
import Text.Show

-- | Data type that represents a full regex
data Regex = Character CharLit   -- ^ Single character match
           | Look Bool Dir Regex -- ^ Lookahead / lookbehind (possibly negated)
           | Position String     -- ^ Positional match / meta-character
           | Many Regex          -- ^ Arbitrary repetition, i.e. x*
           | Some Regex          -- ^ Non-empty repetition, i.e. x+
           | Opt  Regex          -- ^ Optional match, i.e. x?
           | Regex :* Regex      -- ^ Concatenation, i.e. xy
           | Regex :| Regex      -- ^ Alternation, i.e. x|y

infixr 3 :*
infixr 2 :|

data Dir  = Ahead | Behind
data Sign = Positive | Negative

-- | Data type that represents a single character match, including all of its
-- possible forms
data CharLit = AnyChar         -- ^ Any character, i.e. .
             | Single Char     -- ^ Single, naive, literal character
             | Special String  -- ^ Special character group, e.g. \d
             | Group GroupLike -- ^ Character group (possibly negated)

data GroupLike where
    -- | Simple, normal group
    GroupPrim  :: IsSign x => CharGroup x -> GroupLike
    -- | Group difference, i.e. [x-[y]]
    GroupDiff  :: (IsSign x, IsSign y)
               => CharGroup x -> CharGroup y -> GroupLike
    -- | Group intersection, i.e. [x&&[y]]
    GroupInter :: (IsSign x, IsSign y)
               => CharGroup x -> CharGroup y -> GroupLike

newtype CharGroup (s :: Sign) = CharGroup [GroupLit]
    deriving Monoid

-- | Literals valid inside a character group/class
data GroupLit = GSingle Char     -- ^ Literal character
              | GSpecial String  -- ^ Special character
              | GRange Char Char -- ^ Character range, e.g. a-z

instance IsString Regex where
    fromString = foldr1 (:*) . map (Character . Single)

instance s ~ Positive => IsString (CharGroup s) where
    fromString = CharGroup . map GSingle

-- Operator precedence table
prec_or : prec_and : prec_qty : _ = [1..]

instance Show Regex where
    showsPrec _ (Character c) = shows c
    showsPrec _ (Position  p) = showString p

    showsPrec _ (Look neg dir r) = showString "(?" . showDir . showNeg
                                 . shows r . showChar ')'
        where showDir = case dir of Behind -> showChar '<'
                                    Ahead  -> id
              showNeg | neg = showChar '!'
                      | otherwise = showChar '='

    showsPrec d (Many r) = showParen' (d > prec_qty)
            $ showsPrec prec_qty r
            . showChar '*'

    showsPrec d (Some r) = showParen' (d > prec_qty)
            $ showsPrec prec_qty r
            . showChar '+'

    showsPrec d (Opt  r) = showParen' (d > prec_qty)
            $ showsPrec prec_qty r
            . showChar '?'

    showsPrec d (a :* b) = showParen' (d > prec_and)
            $ showsPrec prec_and a
            . showsPrec prec_and b

    showsPrec d (a :| b) = showParen' (d > prec_or)
            $ showsPrec prec_or a
            . showChar '|'
            . showsPrec prec_or b

-- like showParen, but makes non-matching groups
showParen' :: Bool -> ShowS -> ShowS
showParen' True  p = showString "(?:" . p . showChar ')'
showParen' False p = p

-- This always prints non-printable/safe characters as hex escape codes
escape :: [Char] -> Char -> ShowS
escape badChars c
    | c `elem` badChars = showChar '\\' . showChar c
    | isPrint c && isAscii c = showChar c
    | otherwise = showString "\\x{" . showHex (ord c) . showChar '}'
        where showHex = showIntAtBase 16 intToDigit

instance Show CharLit where
    showsPrec _ AnyChar     = showChar '.'
    showsPrec _ (Single c)  = escape "[\\^$.|?*+()" c
    showsPrec _ (Special s) = showChar '\\' . showString s
    showsPrec _ (Group grp) = shows grp

instance Show GroupLike where
    showsPrec _ (GroupPrim x)    = showChar '[' . shows x . showChar ']'
    showsPrec _ (GroupDiff x y)  = showChar '[' . shows x . showString "-["
                                 . shows y . showString "]]"
    showsPrec _ (GroupInter x y) = showChar '[' . shows x . showString "&&["
                                 . shows y . showString "]]"

class ShowSign (s :: Sign) where showSign :: proxy s -> ShowS
instance ShowSign Positive where showSign _ = id
instance ShowSign Negative where showSign _ = showChar '^'

type IsSign = ShowSign

instance IsSign s => Show (CharGroup s) where
    showsPrec _ c@(CharGroup ls) = foldr (.) id $ showSign c : map shows ls

instance Show GroupLit where
    showsPrec _ (GSingle c)  = escape "^-]\\" c
    showsPrec _ (GSpecial c) = showChar '\\' . showString c
    showsPrec _ (GRange a b) = showChar a . showChar '-' . showChar b

-- Helper functions / pre-definitions

charSpecial :: String -> Regex
charSpecial = Character . Special

wordBoundary :: Regex
wordBoundary = Position "\\b"

endOfLine :: Regex
endOfLine = Position "$"

startOfLine :: Regex
startOfLine = Position "^"

look, lookNot :: Dir -> Regex -> Regex
look    = Look False
lookNot = Look True

charClass :: IsSign s => CharGroup s -> Regex
charClass = Character . Group . GroupPrim

classDiff :: (IsSign x, IsSign y) => CharGroup x -> CharGroup y -> Regex
classDiff x = Character . Group . GroupDiff x

classInter :: (IsSign x, IsSign y) => CharGroup x -> CharGroup y -> Regex
classInter x = Character . Group . GroupInter x

inv :: CharGroup Positive -> CharGroup Negative
inv (CharGroup x) = CharGroup x

range :: Char -> Char -> CharGroup s
range a b = CharGroup [GRange a b]

groupSpecial :: String -> CharGroup s
groupSpecial = CharGroup . pure . GSpecial

wordChar :: CharGroup s
wordChar = groupSpecial "w"

digit :: CharGroup s
digit = groupSpecial "d"
