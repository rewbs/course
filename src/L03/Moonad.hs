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
  bind = error "todo"
  reeturn = error "todo"

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
-- apply m(b -> c)
-- ?? :: m c

lift2 f ma = apply (fmaap' f ma)

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
