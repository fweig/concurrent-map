module Main 
    where

import qualified GuardedValueTest

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit


canaryTest = assertBool
    "Canary test should never fail."
    True


tests = hUnitTestToTests $ TestList $ map TestCase 
    ([canaryTest] 
    ++ GuardedValueTest.tests)


main = defaultMain tests

