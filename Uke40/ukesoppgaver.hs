-- A

data Ast = V Int | P Ast Ast | M Ast Ast

eval :: Ast -> Int
eval (V x)= x
eval (P x y) = eval x + eval y
eval (M x y) = eval x * eval y

--B 
evalb :: Ast -> Bool
evalb (V x) = mod (eval (V x)) 2 == 1 -- antar at eksempelet viser feil, mtp hva som er informert tidligere i oppgv (x==1 =True x==0 = False)
evalb (P x y) = (evalb x) || (evalb y)
evalb (M x y) = (evalb x) && (evalb y)

--C
ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (M x y) a b c= c (ev x a b c) (ev y a b c)
ev (P x y) a b c = b (ev x a b c) (ev y a b c)
ev (V x) a b c = a x

vStr x = show x
mStr x y= "("++x ++ "*" ++ y ++")" 
pStr x y=  "(" ++ x ++ "+" ++ y ++ ")"

--eval
mi x y= x * y
pio x y= (x + y)
vi x = x

--evalb
mb x y = x && y
pb x y = x || y
vb x = mod x 2 == 1

--ev t vi pi mi = eval t
--ev t vb pb mb = evalb t

--D
--countNodes (Node (Leaf 1) (Leaf 2)) 0
--countNodes (Node (Node(Leaf 1) (Leaf 2)) (Node(Leaf 1) (Leaf 2))) 0
treeo = Node((Node(Leaf 1)(Node (Leaf 10)(Leaf 11))))
                (Node(Leaf 12)(Leaf 13))

data Tree a = Leaf a | Node (Tree a) (Tree a)

--countNodes (Node a b) c = (countNodes a (c+1) + (countNodes b (c+1)))
countLeaves (Node a b) = countLeaves a + countLeaves b 
countLeaves (Leaf a) = 1

balanced :: Tree a -> Bool
balanced (Node a b) = countLeaves a == countLeaves b || countLeaves a == ((countLeaves b) +1) || ((countLeaves a) + 1) == (countLeaves b)
balaced (Leaf a) = True
--E         
data Expr = Val Int | Add Expr Expr

folde:: (Int-> a) -> (a-> a ->a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

--6
--eval1 :: Expr -> Int
--eval1 (Val x) f g = x
--eval1 (Add x y) f g = folde f g x
--size :: Expr -> Int
