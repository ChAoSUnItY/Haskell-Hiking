{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (intercalate, sortBy)
import Safe (atMay, readMay)

-- If you wants to makes polynomial accepts double instead of int, change the function's signature below!
main :: IO ()
main = (cli :: Stack (Polynomial Int Int) -> IO ()) emptyStack

cli :: (Read a, Num a, Ord a, Show a, Read b, Num b, Ord b, Show b) => Stack (Polynomial a b) -> IO ()
cli stack = do
  command <- getLine
  let commandSegments = words command
      commandIndex = read $ head commandSegments :: Int

  case commandIndex of
    0 -> return ()
    1 -> do
      let filePath = atMay commandSegments 1

      case filePath of
        Just filePath' -> do
          polynomial <- fileAsPolynomial filePath'
          cli $ push polynomial stack
        Nothing -> do
          putStrLn "Unable to read polynomial from file: invalid file content / path"
          cli stack
    2 -> do
      print stack
      cli stack
    3 -> do
      let (lhs, stack') = pop stack
      let (rhs, stack'') = pop stack'
      case (lhs, rhs) of
        (Just poly1, Just poly2) -> cli $ push (addPoly poly1 poly2) stack''
        _ -> do
          putStrLn "Unable to perform polynomial addition: not enough operands"
          cli stack''
    4 -> do
      let mul = atMay commandSegments 1 >>= readMay

      case mul of
        Just mul' -> do
          let (top, stack') = pop stack

          case top of
            Just poly -> cli $ push (singleMulPoly mul' poly) stack'
            Nothing -> do
              putStrLn "Unable to perform single polynomial multiplication: not enough operand"
              cli stack
        Nothing -> do
          putStrLn "Unable to perform single polynomial multiplication: incomplete input"
          cli stack
    5 -> do
      print $ (\(Poly polys) -> snd $ head polys) <$> stack
      cli stack
    6 -> do
      let coef = atMay commandSegments 1 >>= readMay
      let expon = atMay commandSegments 2 >>= readMay
      let (top, stack') = pop stack

      case top of
        Just top' ->
          case (coef, expon) of
            (Just coef', Just expon') ->
              cli $ push (attachPoly top' (coef', expon')) stack'
            _ -> do
              putStrLn "Unable to attach polynomial: invalid arguments"
              cli stack
        Nothing -> do
          putStrLn "Unable to attach polynomial: not enough operand"
          cli stack
    7 -> do
      let expon = atMay commandSegments 1 >>= readMay
      let (top, stack') = pop stack

      case expon of
        Just expon' ->
          case top of
            Just (Poly polys) -> do
              cli $ push (Poly $ filter (\(_, expon'') -> expon'' /= expon') polys) stack'
            Nothing -> do
              putStrLn "Unable to perform polynomial deletion: not enough operand"
              cli stack
        Nothing -> do
          putStrLn "Unable to perform polynomial deletion: invalid argument"
          cli stack
    8 -> do
      let (lhs, stack') = pop stack
      let (rhs, stack'') = pop stack'

      case (lhs, rhs) of
        (Just lhs', Just rhs') -> do
          cli $ push (mulPoly lhs' rhs') stack''
        _ -> do
          putStrLn "Unable to perform polynomial multiplication: not enough operands"
          cli stack
    _ -> do
      putStrLn $ "Invalid command index " ++ show commandIndex
      cli stack

newtype Stack a = Stack [a]

instance (Show a) => Show (Stack a) where
  show :: Stack a -> String
  show (Stack stack) = intercalate "\n" $ ["[" ++ show n ++ "] = " ++ show a | (n, a) <- zip [length stack,length stack - 1..1] stack]

instance Functor Stack where
  fmap :: (a -> b) -> Stack a -> Stack b
  fmap f (Stack inner) = Stack (fmap f inner)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)

newtype (Num a, Num b) => Polynomial a b = Poly [(a, b)] deriving (Eq)

instance (Show a, Num a, Show b, Num b) => Show (Polynomial a b) where
  show :: Polynomial a b -> String
  show (Poly polys) = intercalate " + " $ map (\(coef, expon) -> show coef ++ "^" ++ show expon) polys

addPoly :: (Num a, Ord a, Num b, Ord b) => Polynomial a b -> Polynomial a b -> Polynomial a b
addPoly (Poly polynomials1) (Poly polynomials2) =
  Poly $ addPoly' polynomials1 polynomials2

addPoly' :: (Num a, Ord a, Num b, Ord b) => [(a, b)] -> [(a, b)] -> [(a, b)]
addPoly' [] r = r
addPoly' l [] = l
addPoly' l r
  | le < re = (rc, re) : addPoly' l rs
  | le == re = (lc + rc, le) : addPoly' ls rs
  | le > re = (lc, le) : addPoly' ls r
  where
    ((lc, le) : ls) = l
    ((rc, re) : rs) = r
addPoly' _ _ = []

singleMulPoly :: (Num a, Num b) => a -> Polynomial a b -> Polynomial a b
singleMulPoly mul (Poly polys) = Poly $ map (\(coef, expon) -> (coef * mul, expon)) polys

mulPoly :: (Num a, Num b) => Polynomial a b -> Polynomial a b -> Polynomial a b
mulPoly (Poly polynomials1) (Poly polynomials2) =
  Poly $ map (\((c1, e1), (c2, e2)) -> (c1 * c2, e1 + e2)) combinations
  where
    combinations = [(l, r) | l <- polynomials1, r <- polynomials2]

attachPoly :: (Num a, Num b, Ord b) => Polynomial a b -> (a, b) -> Polynomial a b
attachPoly (Poly poly) mono = Poly $ sortBy sortGT (mono : poly)

sortGT :: (Ord b) => (a, b) -> (a, b) -> Ordering
sortGT (_, exponl) (_, exponr)
  | exponl < exponr = GT
  | exponl > exponr = LT
  | otherwise = GT

fileAsPolynomial :: (Read a, Num a, Read b, Ord b, Num b) => String -> IO (Polynomial a b)
fileAsPolynomial filePath = do
  contents <- readFile filePath
  let numbers = words contents
      monomialPairs = map (bimap read read) $ pairs numbers
  return $ Poly $ sortBy sortGT monomialPairs

-- Utilitie functions

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (l : r : xs) = (l, r) : pairs xs
pairs (_ : _) = []
