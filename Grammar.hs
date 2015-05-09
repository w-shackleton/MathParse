module Grammar where

data E = E :+: T | E :-: T | T T deriving (Eq)
data T = T :*: F | T :/: F | F F deriving (Eq)
data F = Num Int | Paren E deriving (Eq)

instance Show E where
  showsPrec d (e :+: t) = showsPrec d e . showString "+" . showsPrec d t
  showsPrec d (e :-: t) = showsPrec d e . showString "-" . showsPrec d t
  showsPrec d (T t) = showsPrec d t
instance Show T where
  showsPrec d (t :*: f) = showsPrec d t . showString "/" . showsPrec d f
  showsPrec d (t :/: f) = showsPrec d t . showString "/" . showsPrec d f
  showsPrec d (F f) = showsPrec d f
instance Show F where
  showsPrec d (Num x) = showsPrec d x
  showsPrec d (Paren e) = showParen True $ showsPrec d e

class Eval a where
  eval :: a -> Int

instance Eval E where
  eval (l :+: r) = eval l + eval r
  eval (l :-: r) = eval l - eval r
  eval (T t) = eval t
instance Eval T where
  eval (l :*: r) = eval l * eval r
  eval (l :/: r) = eval l `div` eval r
  eval (F f) = eval f
instance Eval F where
  eval (Num x) = x
  eval (Paren e) = eval e
