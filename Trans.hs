module Trans where

{- Transforms expressions from TailRecGrammar to Grammar -}

import qualified TailRecGrammar as H
import qualified Grammar as G
import Grammar(E((:+:),(:-:)),T((:*:),(:/:)))

ftrans :: H.F -> G.F
ftrans (H.Num x) = G.Num x
ftrans (H.Paren e) = G.Paren (etrans e)

ttrans :: H.T -> G.T
ttrans (H.T f t) = ttrans' (G.F $ ftrans f) t

ttrans' :: G.T -> H.T' -> G.T
ttrans' g1 (H.Mul f2 t2) = ttrans' (g1 :*: (ftrans f2)) t2
ttrans' g1 (H.Div f2 t2) = ttrans' (g1 :/: (ftrans f2)) t2
ttrans' g1 H.TNone = g1

etrans :: H.E -> G.E
etrans (H.E t e) = etrans' (G.T $ ttrans t) e

etrans' :: G.E -> H.E' -> G.E
etrans' g1 (H.Plus  t2 e2) = etrans' (g1 :+: (ttrans t2)) e2
etrans' g1 (H.Minus t2 e2) = etrans' (g1 :-: (ttrans t2)) e2
etrans' g1 H.ENone = g1
