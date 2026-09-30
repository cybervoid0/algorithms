module Codewars.Kyu3.TheCataKata where

import Codewars.Kyu3.RecursionSchemesPreloaded
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Numeric.Natural
import Prelude hiding (succ)

-- Let's start with initial requirements for our task
-- We want to abstract the recursion out of our logic code completely,
-- such that our recursion schemes will work with any recursive data structure.
-- Turns out we can actually factor out recursion at data type level!
-- For example, let's imagine a classic example of AST for some language
-- of mathematical expression
--
-- > data Ast
-- >   = Var String
-- >   | Lit Int
-- >   | Add Ast Ast
-- >   | Mul Ast Ast
--
-- Here Ast references itself in Add and Mul constructors. Let's factor that out.

data AstF f
  = Var String
  | Lit Int
  | Add f f
  | Mul f f
  deriving (Show, Eq, Ord)

-- It is also important to notice that our AstF forms a functor!
instance Functor AstF where
  fmap f astf = case astf of
    Var str -> Var str
    Lit num -> Lit num
    Add x y -> Add (f x) (f y)
    Mul x y -> Mul (f x) (f y)

-- Now, to reconstruct our initial Ast from AstF we just need to plug AstF into f!
-- But if you try to write
--
-- > type Ast = AstF Ast
--
-- that wouldn't actually compile. Instead we introduce a newtype wrapper that behaves
-- similarly to well-known `fix` combinator from lambda calculus that returns a fixed point
-- of it's argument. Instead our capitalized Fix is actually a fixed point of a functor!
--
-- Preloaded (see Codewars.Kyu3.RecursionSchemesPreloaded):
-- > newtype Fix f = Fix { unFix :: f (Fix f) }
-- > deriving instance Show (f (Fix f)) => Show (Fix f)
-- > deriving instance Eq (f (Fix f)) => Eq (Fix f)
-- > deriving instance Ord (f (Fix f)) => Ord (Fix f)

-- We call our type AstF a base functor for Ast
type Ast = Fix AstF

-- We also add some smart constructors to help us construct values of type Fix AstF
mkVar :: String -> Ast
mkVar x = Fix (Var x)

mkLit :: Int -> Ast
mkLit x = Fix (Lit x)

mkAdd :: Ast -> Ast -> Ast
mkAdd l r = Fix (Add l r)

mkMul :: Ast -> Ast -> Ast
mkMul l r = Fix (Mul l r)

-- Now it's time to meet our first recursion scheme.
-- A catamorphism is a generalization of fold. Given our base functor F and
-- some function F a -> b, which we call an F-algebra, we transform it into a
-- function Fix F -> b. Meaning, given some way to fold one layer of a structure
-- we can fold the whole structure.
-- Try to implement cata yourself. There's only so many implementations that can actually
-- fit the given type

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata f g = undefined
 where
  foo = unFix g

-- Let's write a couple algebras for our Ast.
-- Count all the nodes in an AST
countAst :: AstF Int -> Int
countAst = undefined

-- Compute the depth of an AST
depthAst :: AstF Int -> Int
depthAst = undefined

-- Pretty print our AST
-- Print variables and literals as is, addition as (l + r), multiplication as l * r
pretty :: AstF String -> String
pretty = undefined

-- Now we can do an optimization technique called constant folding.
-- This algebra must optimize away all operations on constant values
-- So any Lit + Lit or Lit * Lit becomes just one Lit.
-- Also you must optimize all expressions of the form 0 * x, 0 + x, 1 * x
optimize :: AstF Ast -> Ast
optimize = undefined

-- Since our traversal with cata is bottom-up, one pass of optimize would be enough.

-- We can do effects as well!
type Env = M.Map String Int

type Ctx a = MaybeT (Reader Env) a

-- Evaluate expression by pulling bound variables from Env. If variable is unbound, return Nothing
eval :: AstF (Ctx Int) -> Ctx Int
eval = undefined

evalAst :: Env -> Ast -> Maybe Int
evalAst env ast = runReader (runMaybeT (cata eval ast)) env

-- Notice how our actual business logic contains no recursion at all! Neat!

-- Now let's talk about a dual of a fold. In recursion schemes, the generalization of
-- an unfold is called anamorphism. It is a function from an F-coalgebra a -> F a
-- to a -> Fix f. Notice how we obtained it by just flipping the arrows in cata.
-- It allows us, given a way to build one layer, to build the whole, potentially
-- infinite, structure.
-- Again, there's not a lot of different ways to write this function.

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana = undefined

-- Notice how we can plug pretty much any functor into Fix and get cata and ana.
-- For example, if we put Maybe inside of itself, we get a structure isomorphic to Peano
-- representation for natural numbers!

type Nat = Fix Maybe

zero :: Nat
zero = undefined

succ :: Nat -> Nat
succ = undefined

-- Now let's define algebra and coalgebra for converting
-- between our Nat and Natural from base.

-- Fold one layer of Fix Maybe into a Natural
foldNat :: Maybe Natural -> Natural
foldNat = undefined

fromNat = cata foldNat

-- Build one layer of Fix Maybe from a Natural
unfoldNat :: Natural -> Maybe Natural
unfoldNat = undefined

toNat = ana unfoldNat

-- Following this example, write a base functor for List type.

data ListF a f = Something
  deriving (Show, Eq)

type List a = Fix (ListF a)

mkNil :: List a
mkNil = undefined

mkCons :: a -> List a -> List a
mkCons = undefined

instance Functor (ListF a) where
  fmap = undefined

foldList :: ListF a [a] -> [a]
foldList = undefined

fromList = cata foldList

unfoldList :: [a] -> ListF a [a]
unfoldList = undefined

toList = ana unfoldList

-- Let's write a coalgebra that builds an infinite sequence of natural numbers
-- starting from a given number
natsCoalgebra :: Nat -> ListF Nat Nat
natsCoalgebra = undefined

nats :: List Nat
nats = ana natsCoalgebra zero

-- But what if we want to access the current subtree during the fold? We can't conveniently do
-- that with cata since it consumes the subtree right away. Turns out, there's a tool for that!
-- Introducing paramorphism -- a generalization of catamorphisms that allows precisely what we
-- have described, access to the intermediate structure during every step of the fold.
-- para builds a fold Fix F -> a from a step F (Fix F, a) -> a called R-algebra. First element
-- in that product is the subtree in question.

-- This one might be a little bit tricker, but remember, there's only one way to do it.
para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para = undefined

-- Let's do something with our newly obtained power. How about counting all subtrees?
-- This coalgebra should build a mapping from Ast to the number of occurences
countSubtrees :: AstF (Ast, M.Map Ast Int) -> M.Map Ast Int
countSubtrees = undefined

-- Return a map where each key is an Ast that appears more than once and the value is number of occurences
countDups :: Ast -> M.Map Ast Int
countDups = M.filter (> 1) . para countSubtrees

-- What about the dual of paramorphism? Well, if we flip the arrows and replace the product with sum
-- we get this beast that you're about to implement. It is called apomorphism. We have an R-coalgebra
-- of a shape a -> F (Either (Fix F) a). This lets us return either Left, terminating the apomorphism
-- with some final subtree, or continue normally with some seed in Right.

apo :: (Functor f) => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo = undefined

-- With that we can write a small template engine for our language

data Template = Hole String | Mask (AstF Template)
  deriving (Show)

type TemplateEnv = M.Map String Ast

-- Insert an Ast from TemplateEnv at each Hole. If there's no template in the env
-- insert a Var with that Hole identifier
reconstructor :: TemplateEnv -> Template -> AstF (Either Ast Template)
reconstructor = undefined

reconstruct :: TemplateEnv -> Template -> Ast
reconstruct env template = apo (reconstructor env) template

-- Now this classic four of recursion schemes is probably more than enough for what most people
-- would use in daily programming. But I just couldn't omit another layer of generalization on
-- para- and apomorphisms.
-- Last fold we're looking at today is called histomorphism. As the name suggests, it's a
-- recursion scheme with access to it's history. It describes style of recursion that's called
-- Course-of-Value recursion. We're going to define a little helper data type to store
-- computed value on each layer of iteration

-- Preloaded (see Codewars.Kyu3.RecursionSchemesPreloaded):
--
-- > data Attr f a = Attr
-- >   { value   :: a            -- This is what've already folded to
-- >   , history :: f (Attr f a) -- This is every previous folding step
-- >   }
-- > deriving instance (Show (f (Attr f a)), Show a) => Show (Attr f a)

-- Next we're going to define histo. Histo folds with a CV-algebra of shape F (Attr F a) -> a.
-- Keep in mind that there are multiple ways that you could write this function. You should try
-- to implement it in such a way, that does not recompute whole history for each iteration,
--- although, this is not checked.

histo :: (Functor f) => (f (Attr f a) -> a) -> Fix f -> a
histo = undefined

-- With access to history of previous computations we can express recursive formulas
-- such as Fibonacci numbers. To make things a little bit complicated, let's implement
-- an algebra that computes n-th Tribonacci number. They're defined as follows:
-- t(0) = 0
-- t(1) = 0
-- t(2) = 1
-- t(n+3) = t(n+2) + t(n-1) + t(n)
-- If we were to implement such computation with cata or foldr, we would be required to carry around
-- context in a tuple. Attr provides arbitrary access anywhere in the history of our computation.

-- We can make use of our makeshift Nats to fold them with histo
tribonacci :: Maybe (Attr Maybe Natural) -> Natural
tribonacci = undefined

trib :: Natural -> Natural
trib x = histo tribonacci (toNat x)

-- Now let's derive the dual of histo. We reverse the arrows and change products into sums.
-- The dual of our Attr is called CoAttr and it is a sum of just the value and the recursive
-- component. So our CV-coalgebra is a -> F (CoAttr F a). That allows us to decide whether we
-- want to just recurse with Automatic or insert arbitrary subtrees into the future with Manual,
-- hence the name, futumorphism.

data CoAttr f a
  = Automatic a -- Continue unfolding with seed a
  | Manual (f (CoAttr f a)) -- Insert a subtree

-- You may notice that this is exactly the type of a Free monad. Actually, Attr is just a
-- Cofree comonad and recursion-schemes package uses these types instead

futu :: (Functor f) => (a -> f (CoAttr f a)) -> a -> Fix f
futu = undefined

-- With that, for example, we can easily reconstruct a sequence described by it's run-length encoding

-- All Ints here would be >= 1
type RLE a = (a, Int)

unRLE :: List (RLE a) -> ListF a (CoAttr (ListF a) (List (RLE a)))
unRLE = undefined

decodeRLE :: [RLE a] -> [a]
decodeRLE = fromList . futu unRLE . toList
