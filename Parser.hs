data Term = Constant Int | Div Term Term

type M a = State -> [(a, State)]
type State = String

unit :: a -> M a
unit a x = [(a, x)]

(#) :: M a -> (a -> M b) -> M b
(m # k) x = [(b,z) | (a,y) <- m x, (b,z) <- k a y]

item :: M Char
item [] = []
item (a:x) = [(a, x)]

twoItems :: M (Char, Char)
twoItems = item # \a -> item # \b -> unit (a, b)

-- Parses zero elements, always fails
zero :: M a
zero x = []

-- Alternation
(<>) :: M a -> M a -> M a
(m <> n) x = m x ++ n x

test :: M Char
test = item <> item

oneOrTwoItems :: M String
oneOrTwoItems = (item # \a -> unit [a]) <> (item # \a -> item # \b -> unit [a,b])

-- Filtering for a parser
type Predicate a = a -> Bool
(|>) :: M a -> Predicate a -> M a
m |> p = m # \a -> if p a then unit a else zero

-- Literal parsing of a character
lit :: Char -> M Char
lit c = item |> (\a -> a == c)

-- 0 or many parses
iter :: M a -> M [a]
iter m = (m # \a -> iter m # \x -> unit (a:x)) <> unit []

-- Biased choice.  Choose first parser if any, else next
(<|>) :: (Eq a ) => M a -> M a -> M a
(m <|> n) x = if m x /= [] then m x else n x

iter' :: (Eq a ) => M a -> M [a]
iter' m = (m # \a -> iter' m # \x -> unit (a:x)) <|> unit []
