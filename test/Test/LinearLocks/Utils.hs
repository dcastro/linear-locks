module Test.LinearLocks.Utils where

import Data.List qualified as List
import GHC.Stack (HasCallStack)
import Test.Syd

-- | Assert that the given list does NOT have the given infix.
shouldNotContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldNotContain a i = shouldSatisfyNamed a ("doesn't have infix\n" <> ppShow i) (not . List.isInfixOf i)

infix 1 `shouldNotContain`
