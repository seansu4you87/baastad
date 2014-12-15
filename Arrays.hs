data Term = Variable Id | Constant Int | Add Term Term
data Command = Assignment Id Term | Sequence Command Command | Conditional Term Command Command
data Program = Program Command Term

data Array = Array Index Value
type State = Array
type Index = Id
type Id = String
type Value = Int

eval :: Term -> State -> Int
eval (Variable i) x = index i x
eval (Constant a) x = a
eval (Add t u) x = eval t x + eval u x

exec :: Command -> State -> State
exec (Assignment i t) x = update i (eval t x) x
exec (Sequence c d) x = exec d (exec c x)
exec (Conditional t c d) x = if eval t x == 0
                             then exec c x
                             else exec d x

elab :: Program -> Int
elab (Program c t) = eval t (exec c (newarray 0))


newarray :: Val -> Array
newarray v = repeat v

-- index :: Index -> Array -> Val

-- update   :: Ix -> Val -> Arr -> Arr

