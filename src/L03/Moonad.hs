module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  -- bind :: (a -> Id b) -> Id a -> Id b
  -- f :: a -> Id b
  -- x :: Id a
  -- runId x :: runId (Id a) :: a
  -- f a :: Id b
  -- ? :: Id b
  bind f x = f (runId x) 
  reeturn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  bind = flatMap
  reeturn a = a :| Nil

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  -- bind :: (a -> Optional b) -> Optional a -> Optional b
  -- f :: (a -> Optional b)
  -- Full a :: Optional a
  -- ?? :: Optional b
  bind f (Full a) = f a
  bind _ Empty = Empty 
  reeturn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Moonad ((->) t) where
  -- bind :: (a -> (t -> b)) -> (t -> a) -> t -> b
  -- f :: (a -> (t -> b))
  -- ta :: (t -> a)
  -- ?? :: (t -> b)
  bind f ta t = f (ta t) t
  reeturn a = (\_ -> a)

-- Exercise 9
-- Relative Difficulty: 2
instance Moonad IO where
  bind = (=<<)
  reeturn = return

-- Exercise 10
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten = bind id 

-- Exercise 11
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
-- bind   :: (a -> m b) -> m a -> m b
-- fmaap' :: (a -> b) -> m a -> m b
-- mf :: m (a -> b)
-- f  :: a -> b
-- ma
-- 
-- ?? :: m b 
apply mab ma = bind (\f -> fmaap' f ma) mab

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
-- apply :: m (a -> b) -> m a -> m b
-- fmaap' :: (a -> b) -> m a -> m b
-- f :: a -> b -> c
-- ma :: m a
-- mb :: m b
-- fmaap' f ma :: fmaap' (a -> (b -> c)) m a  :: m(b -> c)
-- apply m(b -> c) :: mb -> mc
-- ?? :: m c

lift2 f ma = apply (fmaap' f ma)

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
-- lift2 :: (a -> b -> c) -> m a -> m b -> m c
-- apply :: m (a -> b) -> m a -> m b

-- lift2 f ma mb:: lift2 (a -> b -> (c -> d)) (m a) (m b) :: m(c -> d) 
-- apply m(c -> d) :: mc -> md
lift3 f ma mb = apply (lift2 f ma mb)


-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 f ma mb mc = apply (lift3 f ma mb mc)

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
--seequence = error "todo"
-- :       ::  a -> [a] -> [a]
-- foldr   :: (a -> b -> b) -> b -> [a] -> b
-- lift2   :: (a -> b -> c) -> m a -> m b -> m c

-- foldr : :: [a] -> [a] -> [a]

-- lift2 : :: m a -> m[a] -> m[a]


-- ?? :: m [a]

seequence [] = reeturn []

--        ma : [ma] =  m [a] 
seequence (h : t) = lift2 (:) h (seequence t)

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
-- sequence :: [m a] -> m [a]
-- map :: (a -> b) -> [a] -> [b]

traaverse f l = seequence (map f l) 


-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
-- Int -> m a -> m [a]

-- repl :: Int -> a -> [a]
-- sequence :: [m a] -> m [a]
-- repl i ma :: {i -> ma} -> [ma]
-- seequence (repl i ma) :: sequence ([ma]) :: m [a]

reeplicate i ma = seequence (repl i ma)

repl :: Int -> a -> [a]
repl i a | i <= 0 = []
         | i >  0 = a : (repl (i-1) a)   

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
-- bind :: (a -> m b) -> m a -> m b
-- bind (a -> m Bool) m a -> { (a -> m Bool) -> m a } -> m Bool
-- bind f reeturn (a) == f a == m Bool
-- fmaap' (==) :: m a -> m (a -> Bool)
-- fmaap' (==) m Bool :: 
-- bind (==) :: 

-- lift2 :: (a -> b -> c) -> m a -> m b -> m c
-- lift2 (m Bool == m Bool)  

--fmaap' :: (a -> b) -> m a -> m b


filtering f [] = reeturn []
filtering f (a:as) =  -- bind (\bool -> (bool == True)) (f a)
                      -- same as f a
					 bind (\das -> fmaap' (\b -> if (b ==True) then a:das else das) (f a)) (filtering f as)

--filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
