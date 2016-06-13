module GuardedValueTest (tests)
  where

import GuardedValue

import Test.HUnit


x = Value 0
y = Value 1

tests = [ eqTest1
        , ltTest1
        , gtTest1
        , leTest1
        , geTest1
        , eqTest2
        , ltTest2
        , gtTest2
        , leTest2
        , geTest2
        , eqTest3
        , ltTest3
        , gtTest3
        , leTest3
        , geTest3
        , eqTest4
        , ltTest4
        , gtTest4
        , leTest4
        , geTest4
        , eqTest5
        , ltTest5
        , gtTest5
        , leTest5
        , geTest5
        , eqTest6
        , ltTest6
        , gtTest6
        , leTest6
        , geTest6
        , eqTest7
        , ltTest7
        , gtTest7
        , leTest7
        , geTest7]


minf = (MinInf :: GuardedValue Int)
inf = (MaxInf :: GuardedValue Int)

eqTest1 = assertBool "0 == 0" $ x == x
ltTest1 = assertBool "0 < 0"  $ not $ x < x
gtTest1 = assertBool "0 > 0"  $ not $ x > x
leTest1 = assertBool "0 <= 0" $ x <= x
geTest1 = assertBool "0 >= 0" $ x >= x

eqTest2 = assertBool "0 == 1" $ x /= y
ltTest2 = assertBool "0 < 1"  $ x < y
gtTest2 = assertBool "0 > 1"  $ not $ x > y
leTest2 = assertBool "0 <= 1" $ x <= y
geTest2 = assertBool "0 >= 1" $ not $ x >= y

eqTest3 = assertBool "0 == -oo" $ x /= minf
ltTest3 = assertBool "0 < -oo"  $ not $ x < minf
gtTest3 = assertBool "0 > -oo"  $ x > minf
leTest3 = assertBool "0 <= -oo" $ not $ x <= minf
geTest3 = assertBool "0 >= -oo" $ x >= minf

eqTest4 = assertBool "0 == oo" $ x /= inf
ltTest4 = assertBool "0 < oo"  $ x < inf
gtTest4 = assertBool "0 > oo"  $ not $ x > inf
leTest4 = assertBool "0 <= oo" $ x <= inf
geTest4 = assertBool "0 >= oo" $ not $ x >= inf

eqTest5 = assertBool "-oo == oo" $ minf /= inf
ltTest5 = assertBool "-oo < oo"  $ minf < inf
gtTest5 = assertBool "-oo > oo"  $ not $ minf > inf
leTest5 = assertBool "-oo <= oo" $ minf <= inf
geTest5 = assertBool "-oo >= oo" $ not $ minf >= inf

eqTest6 = assertBool "-oo == -oo" $ minf == minf
ltTest6 = assertBool "-oo < -oo"  $ not $ minf < minf
gtTest6 = assertBool "-oo > -oo"  $ not $ minf > minf
leTest6 = assertBool "-oo <= -oo" $ minf <= minf
geTest6 = assertBool "-oo >= -oo" $ minf >= minf

eqTest7 = assertBool "oo == oo" $ inf == inf
ltTest7 = assertBool "oo < oo"  $ not $ inf < inf
gtTest7 = assertBool "oo > oo"  $ not $ inf > inf
leTest7 = assertBool "oo <= oo" $ inf <= inf
geTest7 = assertBool "oo >= oo" $ inf >= inf
