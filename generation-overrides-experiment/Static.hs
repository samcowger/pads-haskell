{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Static where

-- | Description: Experiments in user customizable data generation in PADS
--
-- See @./Dynamic.hs@ for more comments.
import Data.Typeable ( Typeable, (:~:)(Refl), eqT )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Text.Printf

----------------------------------------------------------------
-- * Notes
--
-- - Overrides for type-parameterized types.
--
--   E.g.
--
--   @
--   [pads|
--     data A a = A a
--     data B a = B { b_a :: A a } |]
--   @.
--
--   The Haskell types this generates look identical to the PADS
--   declaration.
--
--   What does an override for @b_a@ look like? It's a generator for @A
--   a@, which is something of type @PadsGen a -> PadsGen (A a)@.
--
-- - Overrides for term-parameterized types.
--
--   E.g.
--
--   @
--   [pads|
--     data A (n :: Int) = A { a_bytes :: Bytes n }
--     data B = B { b_n :: Int, b_a :: A b_n } |]
--   @.
--
--   The Haskell types here are different from the PADS declarations,
--   because Haskell does not support term parameters. The Haskell
--   types are
--
--   @
--   data A = A { a_bytes :: Bytes }
--   data B = B { b_n :: Int, b_a :: A }
--   @
--
--   What does an override for @b_a@ look like? It's a generator for
--   @A@ that takes an int argument corresponding to the term
--   parameter @b_n@, i.e. something of type @Int -> PadsGen A@.
--
--   - Overrides for nested parameterized types.
--
--   Given a PADS description with nested parameters overriding a
--   specific field could be problematic. E.g. for the PADS decl
--
--   @
--   [pads|
--     data A a = A a
--     data B = B { b_n :: Int, b_a :: A (Bytes b_n) } |]
--   @
--
--   with corresponding Haskell types
--
--   @
--   data A = A a
--   data B = B { b_n :: Int, b_a :: A Bytes }
--   @
--
--   an override for the @b_a@ field has type @PadsGen Bytes ->
--   PadsGen (A Bytes)@, and gives no access to the @Int@ param giving
--   the length of the bytes. Why? Because the generator for @B@ that
--   PADS synthesizes looks like
--
--   @
--   b_genM = do
--     b_n <- int_genM
--     b_a <- a_genM (bytes_genM b_n)
--     return (B b_n b_a)
--   @
--
--   and a user override of generation for the @b_a@ field simply
--   replaces @a_genM@ there with what the user specifies. But there's
--   no way for the user to extract the @b_n@ from the @bytes_genM
--   b_n@.
--
--   However, we can work around this using PADS type synonyms!
--   Consider instead this alternative PADS declaration that defines
--   the same Haskell types, but with different generators:
--
--   @
--   [pads|
--     data A a = A a
--     type ABytes (n :: Int) = A (Bytes n)
--     data B = B { b_n :: Int, b_a :: ABytes b_n } |]
--   @
--
--   The generated Haskell types are equivalent:
--
--   @
--   data A = A a
--   type ABytes = A Bytes
--   data B = B { b_n :: Int, b_a :: ABytes }
--   @
--
--   But the generator types are now different, and given access to
--   the @Int@ parameter. Namely, PADS generates
--
--   @
--   aBytes_genM (n :: Int) = a_genM (bytes_genM n)
--   b_genM = do
--     b_n <- int_genM
--     b_a <- aBytes_genM b_n
--     return (B b_n b_a)
--   @
--
--   and the user can now override the @b_a@ field's generation with
--   something of the type of @aBytes_genM@, i.e. @Int -> PadsGen
--   ABytes@, or equivalently @Int -> PadsGen (A Bytes)@. Now there is
--   an @Int@ argument corresponding to @b_n@, and so the user has
--   complete control again.
--
-- - Overrides for non-record constructors.
--
--   For record constructors we refer to fields by their name. But
--   what about for non-record constructors with no names? For unnamed
--   fields we can use numeric indices.
--
--   E.g. given the PADS declaration
--
--   @
--   [pads|
--   data E a b = L a | M a b | R b |]
--   @
--
--   we could give its field types as
--
--   @
--   type instance FieldTy (E a b) "L.0" = a
--   type instance FieldTy (E a b) "M.0" = a
--   type instance FieldTy (E a b) "M.1" = b
--   type instance FieldTy (E a b) "R.0" = b
--   @
--
--   For built-in types we have to include such instances in the PADS
--   library of course.

----------------------------------------------------------------
-- * TODO
--
-- - Add wild card paths?
--
--   E.g. override generation of all @Int@s, indep of position?
--
-- - Add overrides for distribution of base types?
--
--   Could be achieved using wild card overrides (above).
--
-- - Add overrides for distribution of constructors in sum types?
--
--   Currently constructors are always generated uniformly. E.g. given
--
--   @
--   [pads|
--   data E a b = L a | R b |]
--   @
--
--   PADS produces
--
--   @
--   e_genM a__g b__g = do
--     let dos_aCoM
--           = [do x <- a__g
--                 (return :: a -> PadsGen a) (L x),
--              do x <- b__g
--                 (return :: a -> PadsGen a) (R x)]
--     index <- randNumBound ((length dos) - 1)
--     (dos !! index)
--   @
--
--   We could add a way to override the generation of @index@ there,
--   without having to duplicate the rest of that generator code.
--
-- - Better error messages using GHC custom type error feature?
--
--   E.g. define 'FieldTy' as a class with a functional dependency,
--   and define an overlapping catch all instance that returns a type
--   error saying "there is no such field" or similar.

----------------------------------------------------------------
-- * Types: not necessarily actual PADS types

type PadsGen = IO

runPadsGen :: PadsGen a -> IO a
runPadsGen = id

----------------------------------------------------------------
-- * Hand written PADS library code used in TH-derived generators

-- | Name for field @f@ in data type @t@.
--
-- The format the field name @f@ is expected to be of the form
-- @"<constructor>.<field>"@. For non-record datatypes we can use
-- numeric indices, e.g.
--
-- > Field [Int] "(:).0" -- list head
-- > Field [Int] "(:).1" -- list tail
--
-- We use the type @t@ in addition to the field name @f@, to get
-- globally unique names. Without the type, the field name @f@ could
-- be ambiguous, e.g. if two different modules export identically
-- named constructors with identically named fields.
data Field (t :: *) (f :: Symbol) where
  (:@) :: forall t f ft.
          ( FieldTy t f ~ ft
          , KnownSymbol f
          , Typeable t
          , Typeable ft )
       => Field t f

-- | A path into a data type and a generator for the type at the end
-- of the path.
data Override (t :: *) where
  (:=) :: FieldTy t f ~ ft
       => Field t f
       -> ([Override ft] -> PadsGen ft)
       -> Override t
  (:.) :: FieldTy t f ~ ft
       => Field t f
       -> Override ft
       -> Override t
infixr 5 :.

type family FieldTy (t :: *) (f :: Symbol) :: *

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
override :: forall t f ft. FieldTy t f ~ ft =>
  Field t f -> ([Override ft] -> PadsGen ft) -> [Override t] -> PadsGen ft
override (:@) defaultGen ovs = gen filteredOvs
  where
    gen = case findExactOv ovs of
      Just ovGen -> ovGen
      Nothing -> defaultGen

    findExactOv :: [Override t] -> Maybe ([Override ft] -> PadsGen ft)
    findExactOv [] = Nothing
    findExactOv (ov : ovs') =
      case ov of
        ((:@) :: Field t f') := gen ->
          case eqT @(Field t f) @(Field t f') of
            Nothing -> findExactOv ovs'
            Just Refl -> Just gen
        _ -> findExactOv ovs'

    filteredOvs = filterOvs ovs

    filterOvs :: [Override t] -> [Override ft]
    filterOvs [] = []
    filterOvs (ov : ovs') =
      case ov of
        ((:@) :: Field t f') :. ov' ->
          case eqT @(Field t f) @(Field t f') of
            Just Refl -> ov' : filterOvs ovs'
            Nothing -> filterOvs ovs'
        _ -> filterOvs ovs'

defaultOvs :: [Override t]
defaultOvs = []

genInt :: PadsGen Int
genInt = genIntOvs defaultOvs

genIntOvs :: [Override Int] -> PadsGen Int
genIntOvs _ = return 42

genString :: PadsGen String
genString = genStringOvs defaultOvs

genStringOvs :: [Override String] -> PadsGen String
genStringOvs _ = return "default"

----------------------------------------------------------------
-- * Example of what PADS would generate
--
-- PADS would use TH to generate everything below from
--
-- > [pads|
-- >   data R1 = R1 { x :: Int, y :: Int }
-- >   data R2 = R2 { r1 :: R1, z :: Int }
-- > |]

data R1 = R1 { x :: String , y :: Int } deriving Show
data R2 = R2 { r1 :: R1, z :: Int } deriving Show

type instance FieldTy R1 "R1.x" = String
type instance FieldTy R1 "R1.y" = Int
type instance FieldTy R2 "R2.r1" = R1
type instance FieldTy R2 "R2.z" = Int

genR1 :: PadsGen R1
genR1 = genR1Ovs defaultOvs

genR1Ovs :: [Override R1] -> PadsGen R1
genR1Ovs ovs = do
  x <- override ((:@) @R1 @"R1.x") genStringOvs ovs
  y <- override ((:@) @R1 @"R1.y") genIntOvs ovs
  return R1{..}

genR2 :: PadsGen R2
genR2 = genR2Ovs defaultOvs

genR2Ovs :: [Override R2] -> PadsGen R2
genR2Ovs ovs = do
  r1 <- override ((:@) @R2 @"R2.r1") genR1Ovs ovs
  z <- override ((:@) @R2 @"R2.z") genIntOvs ovs
  return R2{..}

----------------------------------------------------------------
-- * Example of how user can then override generation

myGenR1 :: PadsGen R1
myGenR1 = do
  return R1{ x = "myGenR1", y = 5 }

myGenInt :: PadsGen Int
myGenInt = return 123

myGenString :: PadsGen String
myGenString = return "myGenString"

myGenR2 :: PadsGen R2
myGenR2 = genR2Ovs [ (:@) @R2 @"R2.r1" := const myGenR1 ]

myGenR2' :: PadsGen R2
myGenR2' = genR2Ovs [ (:@) @R2 @"R2.r1" :. (:@) @R1 @"R1.x" := const myGenString
                    , (:@) @R2 @"R2.r1" :. (:@) @R1 @"R1.y" := const myGenInt
                    , (:@) @R2 @"R2.z"                      := const myGenInt ]

main :: IO ()
main = do
  r2 <- runPadsGen myGenR2
  r2' <- runPadsGen myGenR2'
  r2'' <- runPadsGen genR2
  printf "r2 = %s\n" (show r2)
  printf "r2' = %s\n" (show r2')
  printf "r2'' = %s\n" (show r2'')
