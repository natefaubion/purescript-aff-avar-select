module Control.Monad.Aff.AVar.Select
  ( Select
  , put
  , take
  , select
  , select'
  , selectWithDefault
  , selectWithDefault'
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, AVAR, isEmptyVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Parallel (parOneOf)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (fromJust)
import Data.Witherable (wilt)
import Partial.Unsafe (unsafePartial)

data SelectOp a b
  = Put (AVar b) (Unit → b) (Unit → a)
  | Take (AVar b) (b → a)

type SelectOp' a = Exists (SelectOp a)

type RunSelectOp eff a = Aff (avar ∷ AVAR | eff) (Aff (avar ∷ AVAR | eff) a)

data Select r a = Select (Array (SelectOp' r)) a

derive instance functorSelectM ∷ Functor (Select r)

instance applySelect ∷ Apply (Select r) where
  apply (Select r1 f) (Select r2 a) = Select (r1 <> r2) (f a)

instance bindSelect ∷ Bind (Select r) where
  bind (Select r1 a) k = case k a of Select r2 b → Select (r1 <> r2) b

selectVar ∷ ∀ a b. SelectOp a b → AVar b
selectVar = case _ of
  Put avar _ _ → avar
  Take avar _  → avar

-- | Attempts to put a value to an AVar.
put ∷ ∀ a b. AVar a → (Unit → a) → (Unit → b) → Select b Unit
put a k1 k2 = Select (pure (mkExists (Put a k1 k2))) unit

-- | Attempts to take a value from an AVar
take ∷ ∀ a b. AVar a → (a → b) → Select b Unit
take a k = Select (pure (mkExists (Take a k))) unit

runSelectOp
  ∷ ∀ eff a b
  . SelectOp (Aff (avar ∷ AVAR | eff) a) b
  → RunSelectOp eff a
runSelectOp = case _ of
  Put avar k1 k2 → do
    putVar avar (k1 unit)
    pure (k2 unit)
  Take avar k → do
    val ← takeVar avar
    pure (k val)

unsafeRandomIx
  ∷ ∀ eff a
  . Array a
  → Eff (random ∷ RANDOM | eff) a
unsafeRandomIx arr = do
  n ← randomInt 0 (Array.length arr - 1)
  pure (unsafePartial (fromJust (Array.index arr n)))

partitionSelectOps
  ∷ ∀ eff a
  . Array (SelectOp' (Aff (avar ∷ AVAR | eff) a))
  → Aff (avar ∷ AVAR | eff) { left ∷ Array (RunSelectOp eff a), right ∷ Array (RunSelectOp eff a) }
partitionSelectOps = wilt $ runExists \op → do
  let op' = runSelectOp op
  isEmptyVar (selectVar op) <#> if _ then Left op' else Right op'

-- | Attempts to take/put from the set of provided operations. If no operations
-- | can be immediately initiatied, the runtime races them in parallel. If
-- | multiple operations can be initiated, the runtime chooses one at random.
select
  ∷ ∀ eff a
  . Select (Aff (avar ∷ AVAR, random ∷ RANDOM | eff) a) Unit
  → Aff (avar ∷ AVAR, random ∷ RANDOM | eff) a
select (Select cases _) = do
  { left, right } ← partitionSelectOps cases
  if Array.null right
    then join (parOneOf left)
    else join =<< liftEff (unsafeRandomIx right)

-- | Like `select`, but chooses the first available operation rather than
-- | randomly.
select'
  ∷ ∀ eff a
  . Select (Aff (avar ∷ AVAR | eff) a) Unit
  → Aff (avar ∷ AVAR | eff) a
select' (Select cases _) = do
  { left, right } ← partitionSelectOps cases
  if Array.null right
    then join (parOneOf left)
    else join (unsafePartial (fromJust (Array.head right)))

-- | Attempts to take/put from the set of provided operations. If no operation
-- | can be initiated, the default effect will be run. If multiple operations
-- | can be initiated, the runtime chooses one at random.
selectWithDefault
  ∷ ∀ eff a
  . Select (Aff (avar ∷ AVAR, random ∷ RANDOM | eff) a) Unit
  → Aff (avar ∷ AVAR, random ∷ RANDOM | eff) a
  → Aff (avar ∷ AVAR, random ∷ RANDOM | eff) a
selectWithDefault (Select cases _) def = do
  { left, right } ← partitionSelectOps cases
  if Array.null right
    then def
    else join =<< liftEff (unsafeRandomIx right)

-- | Like `selectWithDefault`, but chooses the first available operation rather
-- |than randomly.
selectWithDefault'
  ∷ ∀ eff a
  . Select (Aff (avar ∷ AVAR | eff) a) Unit
  → Aff (avar ∷ AVAR | eff) a
  → Aff (avar ∷ AVAR | eff) a
selectWithDefault' (Select cases _) def = do
  { left, right } ← partitionSelectOps cases
  if Array.null right
    then def
    else join (unsafePartial (fromJust (Array.head right)))
