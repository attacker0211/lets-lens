{-# LANGUAGE RankNTypes #-}

module Lets.Lens (
  fmapT
, over
, fmapTAgain
, Set
, sets
, mapped
, set
, foldMapT
, foldMapOf
, foldMapTAgain
, Fold
, folds
, folded
, Get
, get
, Traversal
, both
, traverseLeft
, traverseRight
, Traversal'
, Lens
, Prism
, _Left
, _Right
, prism
, _Just
, _Nothing
, setP
, getP
, Prism'
, modify
, (%~)
, (.~)
, fmodify
, (|=)
, fstL
, sndL
, mapL
, setL
, compose
, (|.)
, identity
, product
, (***)
, choice
, (|||)
, Lens'
, cityL
, stateL
, countryL
, streetL
, suburbL
, localityL
, ageL
, nameL
, addressL
, intAndIntL
, intAndL
, getSuburb
, setStreet
, getAgeAndCountry
, setCityAndLocality
, getSuburbOrCity
, setStreetOrState
, modifyCityUppercase
, modifyIntAndLengthEven
, traverseLocality
, intOrIntP
, intOrP
, intOrLengthEven
) where

import Control.Applicative(Applicative((<*>), pure))
import Data.Char(toUpper)
import Data.Foldable(Foldable(foldMap))
import Data.Functor((<$>))
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Monoid(Monoid)
import qualified Data.Set as Set(Set, insert, delete, member)
import Data.Traversable(Traversable(traverse))
import Lets.Data(AlongsideLeft(AlongsideLeft, getAlongsideLeft), AlongsideRight(AlongsideRight, getAlongsideRight), Identity(Identity, getIdentity), Const(Const, getConst), Tagged(Tagged, getTagged), IntOr(IntOrIs, IntOrIsNot), IntAnd(IntAnd), Person(Person), Locality(Locality), Address(Address))
import Lets.Choice(Choice(left, right))
import Lets.Profunctor(Profunctor(dimap))
import Prelude hiding (product)

-- $setup
import qualified Data.Map as Map(fromList)
import qualified Data.Set as Set(fromList)
import Data.Bool(bool)
import Data.Char(ord)
import Lets.Data

-- Let's remind ourselves of Traversable, noting Foldable and Functor.
--
-- class (Foldable t, Functor t) => Traversable t where
--   traverse ::
--     Applicative f => 
--     (a -> f b)
--     -> t a
--     -> f (t b)

-- | Observe that @fmap@ can be recovered from @traverse@ using @Identity@.
--
-- /Reminder:/ fmap :: Functor t => (a -> b) -> t a -> t b
fmapT ::
  Traversable t =>
  (a -> b)
  -> t a
  -> t b
fmapT f = getIdentity . traverse (Identity . f)
  -- Identity :: a -> Identity a 

-- | Let's refactor out the call to @traverse@ as an argument to @fmapT@.
over :: 
  ((a -> Identity b) -> s -> Identity t)
  -> (a -> b)
  -> s
  -> t
over f g s = getIdentity (f (Identity . g) s)

-- | Here is @fmapT@ again, passing @traverse@ to @over@.
fmapTAgain ::
  Traversable t =>
  (a -> b)
  -> t a
  -> t b
fmapTAgain = over (traverse)

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
-- (a -> f b) -> t a -> f (t b) -> (a -> b) -> t a -> t b

-- | Let's create a type-alias for this type of function.
type Set s t a b =
  (a -> Identity b)
  -> s
  -> Identity t

-- | Let's write an inverse to @over@ that does the @Identity@ wrapping &
-- unwrapping.
sets ::
  ((a -> b) -> s -> t)
  -> Set s t a b  
sets f = \x -> Identity . (f (getIdentity . x))

mapped ::
  Functor f =>
  Set (f a) (f b) a b
mapped = \ab fa -> Identity ((<$>) (getIdentity . ab) fa)

-- type Set (f a) (f b) a b = (a -> Identity b) -> (f a) -> Identity (f b)
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t

set ::
  Set s t a b
  -> s
  -> b
  -> t
set f s b = getIdentity (f (const (Identity b)) s)

--- Set s t a b = (a -> Identity b) -> s -> Identity t

-- | Observe that @foldMap@ can be recovered from @traverse@ using @Const@.
--
-- /Reminder:/ foldMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
foldMapT ::
  (Traversable t, Monoid b) =>
  (a -> b)
  -> t a
  -> b
foldMapT f = getConst . traverse (Const . f)
-- a -> f b, t a
-- traverse ::  (a -> f b) -> t a -> f (t b)
-- Const :: a -> Const a b -- Const a (t b)

-- | Let's refactor out the call to @traverse@ as an argument to @foldMapT@.
foldMapOf ::
  ((a -> Const r b) -> s -> Const r t)
  -> (a -> r)
  -> s
  -> r
foldMapOf f ar = getConst . (f (Const . ar))  
-- Const :: a -> Const a b
-- foldMapT :: (Monoid b, Traversable t) => (a -> b) -> t a -> b
-- ((a -> Const r b) -> (s -> Const r t)) -> t ((a -> Const r b)) ->  (s -> Const r t)
-- traverse ::  (a -> f b) -> t a -> f (t b)

-- | Here is @foldMapT@ again, passing @traverse@ to @foldMapOf@.
foldMapTAgain ::
  (Traversable t, Monoid b) =>
  (a -> b)
  -> t a
  -> b
foldMapTAgain = foldMapOf (traverse)
-- foldMapOf :: ((a -> Const b b) -> t a -> Const b (t b))
  -- -> (a -> b)
  -- -> t a
  -- -> b
-- traverse :: (Applicative f, Traversable t) => (a -> Const b b) -> t a ->  Const b (t b)

-- | Let's create a type-alias for this type of function.
type Fold s t a b =
  forall r.
  Monoid r =>
  (a -> Const r b)
  -> s
  -> Const r t

-- | Let's write an inverse to @foldMapOf@ that does the @Const@ wrapping &
-- unwrapping.
folds ::
  ((a -> b) -> s -> t)
  -> (a -> Const b a)
  -> s
  -> Const t s
folds f aba = Const . f (getConst . aba)

folded ::
  Foldable f =>
  Fold (f a) (f a) a a
folded = folds foldMap
-- f a -> r
-- (a -> Const r a) -> f a -> Const r (f a)

-- folds ::
--   ((a -> r) -> (f a) -> r)
--   -> (a -> Const r a)
--   -> (f a)
--   -> Const r (f a)

-- Const fa (getConst . ra)
-- (a -> Const r a) -> f a -> Const r (f a)
-- Const :: a -> Const a b
-- getConst :: Const a b -> a
-- ((a -> r) -> (f a) -> r)  
  --  -> (a -> Const r a)
  -- -> (f a)
  -- -> Const r (f a)

-- | @Get@ is like @Fold@, but without the @Monoid@ constraint.
type Get r s a =
  (a -> Const r a)
  -> s
  -> Const r s

get ::
  Get a s a
  -> s
  -> a
get f s = getConst (f Const s)

----

-- | Let's generalise @Identity@ and @Const r@ to any @Applicative@ instance.
type Traversal s t a b =
  forall f.
  Applicative f =>
  (a -> f b)
  -> s
  -> f t

-- | Traverse both sides of a pair.
both ::
  Traversal (a, a) (b, b) a b
both f (a1, a2) =  (,) <$> (f a1) <*> (f a2)
-- (a -> f b) -> (a, a) -> f (b, b)
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

-- | Traverse the left side of @Either@.
traverseLeft ::
  Traversal (Either a x) (Either b x) a b
traverseLeft afb = either ((<$>) Left . afb) (pure . Right)
  --either (\a -> Left <$> afb a) (\x -> Right <$> pure x)

-- traverseLeft afb (Left ax) = Left <$> (afb ax)
-- traverseLeft _ (Right x) = Right <$> (pure x)

-- (a -> f b) -> Either a x -> f (Either b x)
-- either :: (a -> c) -> (b -> c) -> Either a x -> c

-- | Traverse the right side of @Either@.
traverseRight ::
  Traversal (Either x a) (Either x b) a b
traverseRight afb = either (pure . Left) ((<$>) Right . afb)
  --either (\a -> Left <$> pure a) (\x -> Right <$> afb x)

type Traversal' a b =
  Traversal a a b b

----

-- | @Const r@ is @Applicative@, if @Monoid r@, however, without the @Monoid@
-- constraint (as in @Get@), the only shared abstraction between @Identity@ and
-- @Const r@ is @Functor@.
--
-- Consequently, we arrive at our lens derivation:
type Lens s t a b =
  forall f.
  Functor f =>
  (a -> f b)
  -> s
  -> f t

----

-- | A prism is a less specific type of traversal.
type Prism s t a b =
  forall p f.
  (Choice p, Applicative f) =>
  p a (f b)
  -> p s (f t)

-- dimap :: (b -> a) -> (c -> d) -> p a c -> p b d
_Left ::
  Prism (Either a x) (Either b x) a b
_Left = (dimap id (either ((<$>) Left) (pure . Right))) . left
-- p (Either a x) (Either (f b) x) -> p (Either a x) (f (Either b x))
-- ((Either (f b) x) -> (f (Either b x))) 
-- p a (f b) -> p (Either a x) (f (Either b x))
-- left :: Choice p => p a (f b) -> p (Either a x) (Either (f b) x)

-- dimap :: ((Either a x) -> a) -> ((f b) -> (f (Either b x))) 
-- -> p a (f b) -> p (Either a x) (f (Either b x))
-- dimap :: ((Either a x) -> (Either a x)) -> ((Either (f b) x) -> (f (Either b x))) 
--       -> p (Either a x) (Either (f b) x) -> p (Either a x) (f (Either b x))


_Right ::
  Prism (Either x a) (Either x b) a b 
_Right = (dimap id (either (pure . Left) ((<$>) Right))) . right

prism ::
  (b -> t)
  -> (s -> Either t a)
  -> Prism s t a b
prism bt sta = (dimap sta (either pure ((<$>) bt))) . right
-- either (t -> f t) (f b -> f t) -> Either (t f b) -> f t
-- left :: Choice p => p a b -> p (Either a c) (Either b c)
-- right :: Choice p => p a b -> p (Either c a) (Either c b)  
-- (b -> t) -> (s -> Either t a) -> p a (f b) -> p s (f t)
-- dimap :: (s -> a) -> ((f b) -> (f t)) -> p a (f b) -> p s (f t)
-- dimap :: (s -> (Either t a)) -> (Either t (f b) -> (f t)) -> p (Either t a) (Either t (f b)) 
-- -> p s (f t)
-- dimap :: (s -> (Either t a)) -> ((Either t (f b)) -> (f t)) -> p (Either t a) (Either t (f b))  -> p s (f t)

_Just ::
  Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)
-- (Either (Maybe b) a) -> (a -> Either (Maybe b) a) -> Maybe a -> Either (Maybe b) a
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- (Maybe a) -> Either (Maybe b) a
-- (Nothing -> Maybe b) -> (Just a -> a) -> (Maybe a) -> Either (Maybe b) a
-- prism :: (b -> (Maybe b)) -> ((Maybe a) -> Either (Maybe b) a) -> Prism (Maybe a) (Maybe b) a b

_Nothing ::
  Prism (Maybe a) (Maybe a) () ()
_Nothing = prism (const Nothing) (maybe (Left Nothing) (\_ -> Right ()))
-- p () (f ()) -> p (Maybe a) (f (Maybe a))
-- () -> Maybe a -> (Maybe a -> Either (Maybe a) ())
-- Either (Maybe a) () -> (a -> Either (Maybe a) ())
-- Maybe a -> Either (Maybe a) ()

-- prism ::
--   (() -> (Maybe a))
--   -> ((Maybe a) -> Either (Maybe a) ())
--   -> Prism (Maybe a) (Maybe a) () ()

-- type Prism (Maybe a)  (Maybe a)  () () =
--   forall p f.
--   (Choice p, Applicative f) =>
--   p () (f ())
--   -> p (Maybe a) (f (Maybe a))

-- type Prism s t a b = p a (f b) -> p s (f t)

setP ::
  Prism s t a b
  -> s
  -> Either t a
setP p = either Right Left . (p Left)
-- p a (f b) p s (f t)
-- (s -> Either t a) 
-- Either a (t b) -> Either s (t b)
-- (a -> Either t b) 
-- Left :: a - > Either a b
-- ps :: (a -> Either a b) -> (s -> Either a t)
-- Either a t

getP ::
  Prism s t a b
  -> b
  -> t
getP p = getIdentity . getTagged . p . Tagged . Identity 
-- b -> Tagged a b, a -> Identity a
-- b -> (Identity b) -> (Tagged a (Identity b) -> Tagged s (Identity t)
-- p a (f b) -> p s (f t)
type Prism' a b =
  Prism a a b b

-- >>> modify fstL (+1) (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> modify sndL (+1) ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in modify fstL id (x, y) == (x, y)
--
-- prop> let types = (x :: Int, y :: String) in modify sndL id (x, y) == (x, y)
modify ::
  Lens s t a b
  -> (a -> b)
  -> s
  -> t
modify = over

-- | An alias for @modify@.
(%~) ::
  Lens s t a b
  -> (a -> b)
  -> s
  -> t
(%~) =
  modify

infixr 4 %~

-- |
--
-- >>> fstL .~ 1 $ (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> sndL .~ 1 $ ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (fstL .~ z $ (x, y))
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (sndL .~ z $ (x, y))
(.~) ::
  Lens s t a b
  -> b
  -> s
  -> t
(.~) l b s = set l s b

infixl 5 .~

-- |
--
-- >>> fmodify fstL (+) (5 :: Int, "abc") 8
-- (13,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (10, "abc")
-- Just (20,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (11, "abc")
-- Nothing
fmodify ::
  Functor f =>
  Lens s t a b
  -> (a -> f b)
  -> s
  -> f t 
fmodify l f = l f

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) ::
  Functor f =>
  Lens s t a b
  -> f b
  -> s
  -> f t
(|=) l fb s = (<$>) (set l s) fb

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc")
fstL ::
  Lens (a, x) (b, x) a b
fstL = (\fab (a, x) -> (<$>) (\t -> (t, x)) (fab a))
-- (a -> f b) -> (a, x) -> f (b, x)

-- |
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
sndL ::
  Lens (x, a) (x, b) a b
sndL = (\f (x, a) -> (<$>) (\t -> (x, t)) (f a))

-- |

-- >>> get (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Just 'c'
--
-- >>> get (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Nothing
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'X'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(33,'X')]
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
mapL ::
  Ord k =>
  k
  -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
mapL k = \fa m -> (maybe (Map.delete k m) (\x -> Map.insert k x m)) <$> (fa (Map.lookup k m))
--
-- To work on `Map k a`:
--   Map.lookup :: Ord k => k -> Map k a -> Maybe a
--   Map.insert :: Ord k => k -> a -> Map k a -> Map k a
--   Map.delete :: Ord k => k -> Map k a -> Map k a
--
-- |
--
-- To work on `Set a`:
--   Set.insert :: Ord a => a -> Set a -> Set a
--   Set.member :: Ord a => a -> Set a -> Bool
--   Set.delete :: Ord a => a -> Set a -> Set a
--
-- >>> get (setL 3) (Set.fromList [1..5])
-- True
--
-- >>> get (setL 33) (Set.fromList [1..5])
-- False
--
-- >>> set (setL 3) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5]
--
-- >>> set (setL 3) (Set.fromList [1..5]) False
-- fromList [1,2,4,5]
--
-- >>> set (setL 33) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5,33]
--
-- >>> set (setL 33) (Set.fromList [1..5]) False
-- fromList [1,2,3,4,5]
setL ::
  Ord k =>
  k
  -> Lens (Set.Set k) (Set.Set k) Bool Bool
setL k = \fb s -> (\b -> if b then Set.insert k s else Set.delete k s) <$> (fb (Set.member k s))

-- |
--
-- >>> get (compose fstL sndL) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose fstL sndL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose ::
  Lens s t a b
  -> Lens q r s t
  -> Lens q r a b
compose l1 l2 = (\x -> l2 (l1 x))

-- | An alias for @compose@.
(|.) ::
  Lens s t a b
  -> Lens q r s t
  -> Lens q r a b
(|.) =
  compose

infixr 9 |.

-- |
--
-- >>> get identity 3
-- 3
--
-- >>> set identity 3 4
-- 4
identity ::
  Lens a b a b
identity = id

-- |
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product ::
  Lens s t a b
  -> Lens q r c d
  -> Lens (s, q) (t, r) (a, c) (b, d)
product l1 l2 = (\f (s, q) -> (\(b, d) -> (set l1 s b, set l2 q d)) <$> f (foldMapOf l1 id s, foldMapOf l2 id q))
  -- (set l1 s b, set l2 q d)) <$> _)

--  l2 :: (c -> f d) -> q -> f r 
-- l1 :: (a -> f b) -> s -> f t
-- ((a, c) -> f (b, d)) -> (s, q) -> f (t, r)
-- foldMapOf ::
--   ((a -> Const a b) -> s -> Const a t)
--   -> (a -> a)
--   -> s
--   -> a
-- 
-- product l1 l2 = Lens (\f (s, q) -> (\(b, d) -> (set l1 s b, set l2 q d)) <$> f (get l1 s, get l2 q))

-- | An alias for @product@.
(***) ::
  Lens s t a b
  -> Lens q r c d
  -> Lens (s, q) (t, r) (a, c) (b, d)
(***) =
  product

infixr 3 ***

-- |
--
-- >>> get (choice fstL sndL) (Left ("abc", 7))
-- "abc"
--
-- >>> get (choice fstL sndL) (Right ("abc", 7))
-- 7
--
-- >>> set (choice fstL sndL) (Left ("abc", 7)) "def"
-- Left ("def",7)
--
-- >>> set (choice fstL sndL) (Right ("abc", 7)) 8
-- Right ("abc",8)
choice ::
  Lens s t a b
  -> Lens q r a b
  -> Lens (Either s q) (Either t r) a b
choice l1 l2 =  (\fab x -> either (\s -> Left <$> (l1 fab s)) (\q -> Right <$> (l2 fab q)) x)

-- | An alias for @choice@.
(|||) ::
  Lens s t a b
  -> Lens q r a b
  -> Lens (Either s q) (Either t r) a b
(|||) =
  choice

infixr 2 |||

----

type Lens' a b =
  Lens a a b b

cityL ::
  Lens' Locality String
cityL p (Locality c t y) =
  fmap (\c' -> Locality c' t y) (p c)

stateL ::
  Lens' Locality String
stateL p (Locality c t y) =
  fmap (\t' -> Locality c t' y) (p t)

countryL ::
  Lens' Locality String
countryL p (Locality c t y) =
  fmap (\y' -> Locality c t y') (p y)

streetL ::
  Lens' Address String
streetL p (Address t s l) =
  fmap (\t' -> Address t' s l) (p t)

suburbL ::
  Lens' Address String
suburbL p (Address t s l) =
  fmap (\s' -> Address t s' l) (p s)

localityL ::
  Lens' Address Locality
localityL p (Address t s l) =
  fmap (\l' -> Address t s l') (p l)

ageL ::
  Lens' Person Int
ageL p (Person a n d) =
  fmap (\a' -> Person a' n d) (p a)

nameL ::
  Lens' Person String
nameL p (Person a n d) =
  fmap (\n' -> Person a n' d) (p n)

addressL ::
  Lens' Person Address
addressL p (Person a n d) =
  fmap (\d' -> Person a n d') (p d)

intAndIntL ::
  Lens' (IntAnd a) Int
intAndIntL p (IntAnd n a) =
  fmap (\n' -> IntAnd n' a) (p n)

-- lens for polymorphic update
intAndL ::
  Lens (IntAnd a) (IntAnd b) a b
intAndL p (IntAnd n a) =
  fmap (\a' -> IntAnd n a') (p a)

-- |
--
-- >>> getSuburb fred
-- "Fredville"
--
-- >>> getSuburb mary
-- "Maryland"
getSuburb ::
  Person
  -> String
getSuburb = get $ suburbL |. addressL

-- |
--
-- >>> setStreet fred "Some Other St"
-- Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setStreet mary "Some Other St"
-- Person 28 "Mary" (Address "Some Other St" "Maryland" (Locality "Mary Mary" "Western Mary" "Maristan"))
setStreet ::
  Person
  -> String
  -> Person
setStreet = set $ streetL |. addressL

-- |
--
-- >>> getAgeAndCountry (fred, maryLocality)
-- (24,"Maristan")
--
-- >>> getAgeAndCountry (mary, fredLocality)
-- (28,"Fredalia")
getAgeAndCountry ::
  (Person, Locality)
  -> (Int, String)
getAgeAndCountry = get $ ageL *** countryL

-- |
--
-- >>> setCityAndLocality (fred, maryAddress) ("Some Other City", fredLocality)
-- (Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "Some Other City" "New South Fred" "Fredalia")),Address "83 Mary Ln" "Maryland" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setCityAndLocality (mary, fredAddress) ("Some Other City", maryLocality)
-- (Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "Some Other City" "Western Mary" "Maristan")),Address "15 Fred St" "Fredville" (Locality "Mary Mary" "Western Mary" "Maristan"))
setCityAndLocality ::
  (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality = set $ (cityL |. localityL |. addressL) *** localityL
  
-- |
--
-- >>> getSuburbOrCity (Left maryAddress)
-- "Maryland"
--
-- >>> getSuburbOrCity (Right fredLocality)
-- "Fredmania"
getSuburbOrCity ::
  Either Address Locality
  -> String
getSuburbOrCity = get (suburbL ||| cityL)

-- |
--
-- >>> setStreetOrState (Right maryLocality) "Some Other State"
-- Right (Locality "Mary Mary" "Some Other State" "Maristan")
--
-- >>> setStreetOrState (Left fred) "Some Other St"
-- Left (Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia")))
setStreetOrState ::
  Either Person Locality
  -> String
  -> Either Person Locality
setStreetOrState = set (streetL |. addressL ||| stateL)

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase ::
  Person
  -> Person
modifyCityUppercase = cityL |. localityL |. addressL %~ map toUpper

-- |
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntAndLengthEven ::
  IntAnd [a]
  -> IntAnd Bool
modifyIntAndLengthEven = intAndL %~ (even . length)

---- (a -> f b) -> IntAnd a -> f (IntAnd b)
-- (%~) :: Lens s t a b -> (a -> b) -> s -> t
-- ([a] -> Bool) -> IntAnd [a] -> f (IntAnd Bool)
-- |
--
-- >>> over traverseLocality (map toUpper) (Locality "abc" "def" "ghi")
-- Locality "ABC" "DEF" "GHI"
traverseLocality ::
  Traversal' Locality String
traverseLocality f (Locality l1 l2 l3) = Locality <$> f l1 <*> f l2 <*> f l3
-- (String -> f String) -> Locality -> f Locality
-- Locality :: String -> String -> String -> Locality
--
-- >>> over intOrIntP (*10) (IntOrIs 3)
-- IntOrIs 30
--
-- >>> over intOrIntP (*10) (IntOrIsNot "abc")
-- IntOrIsNot "abc"
intOrIntP ::
  Prism' (IntOr a) Int
intOrIntP = prism IntOrIs (\x -> case x of IntOrIsNot a -> Left (IntOrIsNot a)
                                           IntOrIs a -> Right a)
-- p Int (f Int) -> p (IntOr a) (f (IntOr a))

  -- data IntOr a = IntOrIs Int | IntOrIsNot a
  -- IntOrIs :: Int -> IntOr a
  -- IntOrIsNot :: a -> IntOr a
-- prism ::
--   (Int -> (IntOr a))
--   -> ((IntOr a) -> Either (IntOr a) Int)
--   -> Prism (IntOr a) (IntOr a) Int Int

intOrP ::
  Prism (IntOr a) (IntOr b) a b
intOrP = prism IntOrIsNot (\x -> case x of IntOrIsNot a -> Right a
                                           IntOrIs b -> Left (IntOrIs b))
-- prism :: (b -> (IntOr b)) -> ((IntOr a) -> Either (IntOr b) a) -> Prism (IntOr a) (IntOr b) a b

-- |
--
-- >> intOrLengthEven (IntOrIsNot "abc")
-- IntOrIsNot False
--
-- >>> intOrLengthEven (IntOrIsNot "abcd")
-- IntOrIsNot True
--
-- >>> intOrLengthEven (IntOrIs 10)
-- IntOrIs 10
intOrLengthEven ::
  IntOr [a]
  -> IntOr Bool
intOrLengthEven = over intOrP (even . length)
-- (t0 a0 -> f Bool) -> IntOr [a] -> f (IntOr Bool)
-- ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t