module Orb where

data Space = Op Operation
           | Val Int
    deriving Show

data Operation = Add | Sub | Mul
    deriving (Eq, Show)

data Eval = Lit Int
          | Deferred Operation Int
    deriving (Eq, Show)

newtype Pos = Pos (Int, Int)
    deriving (Eq, Show)

type Path = ([Pos], Eval)

combine :: Space -> Eval -> Eval
combine (Op o) (Lit i)           = Deferred o i
combine (Val i) (Deferred Add c) = Lit (c + i)
combine (Val i) (Deferred Sub c) = Lit (c - i)
combine (Val i) (Deferred Mul c) = Lit (c * i)
combine _ _                      = error "invalid path"

evalPath :: Path -> Eval
evalPath (ps, _) = foldr (combine . getPos) (Deferred Add 0) ps

maze :: [[Space]]
maze = [ [ Op Mul, Val 8, Op Sub, Val 1 ]
       , [ Val 4, Op Mul, Val 11, Op Mul ]
       , [ Op Add, Val 4, Op Sub, Val 18 ]
       , [ Val 22, Op Sub, Val 9, Op Mul ]
       ]


start :: Path
start = ([Pos (3, 0)], Lit 22)

end :: Pos
end = Pos (0, 3)

getPos :: Pos -> Space
getPos (Pos (x, y)) = maze !! x !! y

explore :: Path -> [Path]
explore ([], _) = []
explore (p@(Pos (x, y)) : ps, acc) =
    let ns = [ Pos (x - 1, y), Pos (x + 1, y), Pos (x, y - 1), Pos (x, y + 1) ]
        validP (Pos (a, b)) = and [ a >= 0
                                  , a <= 3
                                  , b >= 0
                                  , b <= 3
                                  , (a, b) /= (3, 0)
                                  ]
        newP = filter validP ns
        cand = map (\np -> (np : p : ps, combine (getPos np) acc)) newP
    in
        filter (\(_, acc) -> case acc of
                    Lit x -> x > 0
                    _     -> True)
               cand

search :: [Path]
search = go [ start ]
  where
    done (h:ps, acc) = h == end && acc == Lit 30
    done (_, _)      = False
    go frontier = let completed = filter done frontier in
        case completed of
        [] -> go $ concatMap explore frontier
        _  -> completed
