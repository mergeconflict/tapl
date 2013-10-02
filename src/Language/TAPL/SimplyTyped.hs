{-# LANGUAGE ViewPatterns #-}

module Language.TAPL.SimplyTyped (evaluate, typecheck) where

{- types -}

data Type = Arr Type Type
          | Bool
  deriving Eq

instance Show Type where
  show (Arr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show Bool      = "Bool"

{- terms -}

data Term = Var Int
          | Abs String Type Term
          | App Term Term
          | If Term Term Term
          | T
          | F
  deriving Eq

instance Show Term where
  show tm =
    let fresh names name
          | name `elem` names = fresh names $ name ++ "'"
          | otherwise         = (name : names, name)
        go names (Var index) =
          names !! index
        go names (Abs name ty body) =
          let (names', name') = fresh names name
          in "λ" ++ name' ++ ":" ++ show ty ++ "." ++ go names' body
        go names (App fn arg) =
          "(" ++ go names fn ++ " " ++ go names arg ++ ")"
        go names (If pred lhs rhs) =
          "{ if " ++ show pred ++ " then " ++ show lhs ++ " else " ++ show rhs ++ " }"
        go _ T = "T"
        go _ F = "F"
    in go [] tm

{- type checking -}

typecheck :: Term -> Maybe Type
typecheck tm =
  let typecheck' types tm = go tm where
        go (Var index) = Just $ types !! index
        go (Abs _ ty (typecheck' $ ty : types -> Just body)) = Just $ Arr ty body
        go (App (go -> Just (Arr a b)) (go -> Just arg)) | a == arg = Just b
        go (If (go -> Just Bool) (go -> Just lhs) (go -> Just rhs)) | lhs == rhs = Just lhs
        go T = Just Bool
        go F = Just Bool
        go _ = Nothing
  in typecheck' [] tm

{- evaluation -}

tweak :: (Int -> Int -> Term) -> Term -> Term
tweak f tm =
  let tweak' n tm = go tm where
        go (Var index)        = f index n
        go (Abs name ty body) = Abs name ty $ tweak' (n + 1) body
        go (App fn arg)       = App (go fn) (go arg)
        go (If pred lhs rhs)  = If (go pred) (go lhs) (go rhs)
        go tm                 = tm
  in tweak' 0 tm

shift :: Int -> Term -> Term
shift distance tm =
  let go index cutoff
        | index >= cutoff = Var $ index + distance
        | otherwise       = Var index
  in tweak go tm

subst :: Int -> Term -> Term -> Term
subst index substitute tm =
  let go index' offset
        | index' == index + offset = shift offset substitute
        | otherwise                = Var index'
  in tweak go tm

evaluate :: Term -> Term
evaluate tm =
  let go (App (Abs _ _ body) arg@(Abs _ _ _)) = shift (-1) (subst 0 (shift 1 arg) body)
      go (App fn@(Abs _ _ _) arg)             = App fn (go arg)
      go (App fn arg)                         = App (go fn) arg
      go tm                                   = tm
      tm'                                     = go tm
  in if tm' == tm then tm else evaluate tm'
