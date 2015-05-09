module TailRecGrammar where

data E = E T E' deriving (Eq, Show)
data E' = Plus T E' | Minus T E' | ENone deriving (Eq, Show)
data T = T F T' deriving (Eq, Show)
data T' = Mul F T' | Div F T' | TNone deriving (Eq, Show)
data F = Num Int | Paren E deriving (Eq, Show)
