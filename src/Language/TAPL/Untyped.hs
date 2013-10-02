module Language.TAPL.Untyped (evaluate) where

data Term = Var Int
          | Abs String Term
          | App Term Term
  deriving Eq

instance Show Term where
  show term =
    let fresh names name
          | name `elem` names = fresh names $ name ++ "'"
          | otherwise         = (name : names, name)
        go names (Var index) =
          names !! index
        go names (Abs name body) =
          let (names', name') = fresh names name
          in "λ" ++ name' ++ "." ++ go names' body
        go names (App lhs rhs) =
          "(" ++ go names lhs ++ " " ++ go names rhs ++ ")"
    in go [] term

shift :: Int -> Term -> Term
shift distance term =
  let go cutoff v@(Var index)
        | index >= cutoff       = Var $ index + distance
        | otherwise             = v
      go cutoff (Abs name body) = Abs name $ go (cutoff + 1) body
      go cutoff (App lhs rhs)   = App (go cutoff lhs) (go cutoff rhs)
  in go 0 term

subst :: Int -> Term -> Term -> Term
subst index substitute term =
  let go offset v@(Var index')
        | index' == index + offset = shift offset substitute
        | otherwise                = v
      go offset (Abs name body)    = Abs name $ go (offset + 1) body
      go offset (App lhs rhs)      = App (go offset lhs) (go offset rhs)
  in go 0 term

evaluate :: Term -> Term
evaluate term =
  let go (App (Abs _ body) rhs@(Abs _ _)) = shift (-1) (subst 0 (shift 1 rhs) body)
      go (App lhs@(Abs _ _) rhs)          = App lhs (go rhs)
      go (App lhs rhs)                    = App (go lhs) rhs
      go term                             = term
      term'                               = go term
  in if term' == term then term else evaluate term'
