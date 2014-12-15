data Term = Con Int | Div Term Term deriving (Show)

-- Identity Monad
type I a = a
-- unit :: a -> I a
-- unit a = a
-- (#) :: I a -> (a -> I b) -> I b
-- a # k = k a

-- Exception Monad
data E a = Raise Exception | Return a deriving (Show)
type Exception = String
-- unit :: a -> E a
-- unit a = Return a
-- (#) :: E a -> (a -> E b) -> E b
-- (Raise e) # k = Raise e
-- (Return a) # k = k a
raise :: Exception -> E a
raise e = Raise e

-- State Monad
type S a = State -> (a, State)
type State = Int
-- unit :: a -> S a
-- unit a = \x -> (a, x)
-- (#) :: S a -> (a -> S b) -> S b
-- s # k = \x -> let (a, y) = s x in
--               let (b, z) = k a y in
--               (b, z)
tick :: S ()
tick = \x -> ((), x+1)

-- Output Monad
type O a = (Output, a)
type Output = String
unit :: a -> O a
unit a = ("", a)
(#) :: O a -> (a -> O b) -> O b
o # k = let (x, a) = o in
        let (y, b) = k a in
        (x ++ y, b)
out :: Output -> O ()
out x = (x, ())

eval :: Term -> O Int
eval (Con a) = out(line (Con a) a) # \_ -> unit a
eval (Div t u) = eval t # (\a -> (eval u # (\b -> out (line (Div t u) (div a b)) # \_ -> unit (div a b))))

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"

answer, bad :: Term
answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
bad = (Div (Con 1 ) (Con 0 ))
