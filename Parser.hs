module Parser (parse) where

import qualified TailRecGrammar as H
import Lexer
import Control.Monad.Trans.State.Lazy
import Control.Monad

type Parse = State (Token, [Token])

parse :: String -> H.E
parse s = evalState (advance >> e) (undefined, mathLex s)

advance :: Parse ()
advance = do
  (_, x:xs) <- get
  put (x, xs)

getToken :: Parse Token
getToken = liftM fst $ get

eat :: Token -> Parse ()
eat t = do
  t' <- getToken
  if t == t' then advance else error ("Failed to parse - found " ++ show t' ++ " expected " ++ show t)

e :: Parse H.E
e = liftM2 H.E t e'

e' :: Parse H.E'
e' = do
  tok <- getToken
  case tok of
    Plus -> eat Plus >> liftM2 H.Plus t e'
    Minus -> eat Minus >> liftM2 H.Minus t e'
    _ -> return H.ENone

t :: Parse H.T
t = liftM2 H.T f t'

t' :: Parse H.T'
t' = do
  tok <- getToken
  case tok of
    Mul -> eat Mul >> liftM2 H.Mul f t'
    Div -> eat Div >> liftM2 H.Div f t'
    _ -> return H.TNone

f :: Parse H.F
f = do
  tok <- getToken
  case tok of
    Num x -> eat (Num x) >> return (H.Num x)
    LParen -> do
      eat LParen
      val <- e
      eat RParen
      return $ H.Paren val
