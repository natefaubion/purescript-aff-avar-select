module Test.Main where

import Prelude

import Control.Monad.Aff (delay, forkAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, tryPutVar)
import Control.Monad.Aff.AVar.Select (select, take)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Data.Time.Duration (Milliseconds(..))

main ∷ ∀ eff. Eff _ Unit
main = launchAff_ do
  var1 ∷ AVar Int ← makeEmptyVar
  var2 ∷ AVar Int ← makeEmptyVar
  var3 ∷ AVar Int ← makeEmptyVar

  let
    loop n
      | n > 10 = pure unit
      | otherwise = do
          select do
            take var1 \n' → do
              log $ "Took var1: " <> show n'
              _ ← tryPutVar var2 (n + 1)
              _ ← tryPutVar var3 (n + 1)
              loop (n + 1)
            take var2 \n' → do
              log $ "Took var2: " <> show n'
              _ ← tryPutVar var1 (n + 1)
              _ ← tryPutVar var3 (n + 1)
              loop (n + 1)
            take var3 \n' → do
              log $ "Took var3: " <> show n'
              _ ← tryPutVar var1 (n + 1)
              _ ← tryPutVar var2 (n + 1)
              loop (n + 1)
  _ ← forkAff $ loop 1
  putVar var1 1
