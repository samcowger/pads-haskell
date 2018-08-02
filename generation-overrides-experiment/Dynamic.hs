{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Description: Experiments in user customizable data generation in PADS
--
-- Experiments with allowing users to customize data generation by
-- overriding default generators. The primary goal is to make the
-- interface easy to use: e.g. if you want to override the generation
-- of one field deep down in a nested record structure you should not
-- need to write much code. The secondary goal is to make it type
-- safe, which should also help with ease of use.
--
-- Three versions of overrides for generation:
--
-- 1) totally dynamic -- i.e. cast overridden generator at runtime --
-- with string keys. No static type checking.
--
-- 2) dynamic paths with statically checked overridden generator
-- functions. Only the generators are checked statically, but the rest
-- of the path could be wrong.
--
-- 3) statically checked paths and statically checked overridden
-- generator functions.
--
-- Use "<record>.<field>" format for path components, to reduce
-- collisions. The components will have to be globally unique if we
-- use type families? Or perhaps we could use the actual types as
-- well?

import Text.Printf

import           Data.Dynamic ( Dynamic, Typeable )
import qualified Data.Dynamic as Dyn
import qualified Type.Reflection as Dyn

----------------------------------------------------------------
-- * Types: not necessarily actual PADS types

type PadsGen = IO
type Field = String
type Path = [Field]
type Override = (Path, Dynamic)

runPadsGen :: PadsGen a -> IO a
runPadsGen = id

----------------------------------------------------------------
-- * Hand written PADS library code used in TH-derived generators
--
-- Totally dynamic version.

defaultOvs :: [Override]
defaultOvs = []

genInt :: PadsGen Int
genInt = genIntOvs defaultOvs

genIntOvs :: [Override] -> PadsGen Int
genIntOvs _ = return 42

-- | Lookup the override for the given field, if any, and use the
-- default generator otherwise.
--
-- 1) The whole field has been overridden directly.
--
-- 2) The field has been overridden indirectly by overriding some of
--    its subfields
--
-- 3) The field is completely default and has not been overridden at
--    all.
--
-- Cases (2) and (3) are actually treated the same, since in both
-- cases we just pass the list of relevant overrides down the call
-- tree; in case (3) the override list just happens to be empty.
override :: forall a. Typeable a => Field -> ([Override] -> PadsGen a) -> [Override] -> PadsGen a
override field defaultGen ovs = gen filteredOvs
  where
    -- The overrides corresponding to the given field, with their
    -- paths specialized to that field (i.e. the given field is
    -- stripped from the paths).
    filteredOvs =
      [ (path', gen) | ((field':path'), gen) <- ovs, field' == field ]
    gen = case lookup [] filteredOvs of
      Nothing -> defaultGen
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error $ printf "override: ill typed override for field %s:\nexpected: %s\ngot: %s"
                   (show field) (show $ Dyn.typeRep @([Override] -> PadsGen a)) (show $ Dyn.dynTypeRep dyn)
        Just gen -> gen

----------------------------------------------------------------
-- * Example of what PADS would generate
--
-- PADS would use TH to generate everything below from
--
-- > [pads|
-- >   data R1 = R1 { x :: Int, y :: Int }
-- >   data R2 = R2 { r1 :: R1, z :: Int }
-- > |]

data R1 = R1 { x :: Int , y :: Int } deriving Show
data R2 = R2 { r1 :: R1, z :: Int } deriving Show

genR1 :: PadsGen R1
genR1 = genR1Ovs defaultOvs

genR1Ovs :: [Override] -> PadsGen R1
genR1Ovs ovs = do
  x <- override "R1.x" genIntOvs ovs
  y <- override "R1.y" genIntOvs ovs
  return R1{..}

genR2 :: PadsGen R2
genR2 = genR2Ovs defaultOvs

genR2Ovs :: [Override] -> PadsGen R2
genR2Ovs ovs = do
  r1 <- override "R2.r1" genR1Ovs ovs
  z <- override "R2.z" genIntOvs ovs
  return R2{..}

----------------------------------------------------------------
-- * Example of how user can then override generation

-- | Turn a generator that doesn't take overrides into one that does
-- take overrides, but ignores them. Can't just use 'const' at use
-- sits, since the 'Typeable' constraint on 'Dyn.toDyn' can't handle
-- polymorphism.
ignoreOvs :: PadsGen a -> [Override] -> PadsGen a
ignoreOvs = const

mkOv :: Typeable a => PadsGen a -> Dynamic
mkOv = Dyn.toDyn . ignoreOvs

myGenR1 :: PadsGen R1
myGenR1 = do
  return R1{ x = 3, y = 5 }

myGenInt :: PadsGen Int
myGenInt = return 123

myGenR2 :: PadsGen R2
myGenR2 = genR2Ovs [ (["R2.r1"], mkOv myGenR1) ]

myGenR2' :: PadsGen R2
myGenR2' = genR2Ovs [ (["R2.r1", "R1.x"], mkOv myGenInt)
                    , (["R2.r1", "R1.y"], mkOv myGenInt)
                    , (["R2.z"],          mkOv myGenInt) ]

main :: IO ()
main = do
  r2 <- runPadsGen myGenR2
  r2' <- runPadsGen myGenR2'
  r2'' <- runPadsGen genR2
  printf "r2 = %s\n" (show r2)
  printf "r2' = %s\n" (show r2')
  printf "r2'' = %s\n" (show r2'')

-- | See error message when a bad override is given:
--
-- > *** Exception: override: ill typed override for field "R1.x":
-- > expected: [([[Char]],Dynamic)] -> IO Int
-- > got: [([[Char]],Dynamic)] -> IO R1
mainError :: IO ()
mainError = do
  r2 <- runPadsGen $ genR2Ovs [ (["R2.r1", "R1.x"], mkOv myGenR1{-should be e.g. 'myGenInt'-}) ]
  printf "r2 = %s\n" (show r2)
