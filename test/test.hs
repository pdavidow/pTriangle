import Test.Tasty
import Test.Tasty.HUnit

import PTriangle (pTriangle, toRowMajor)
import Data.Either (fromRight)

main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests = testGroup "Unit tests" $

    [ testGroup "Pascal's Triangle" $  
        [ testCase "depth 1" $
            (toRowMajor $ (fromRight [[]] $ pTriangle 1)) @?= "1"  

        , testCase "depth 2" $
            (toRowMajor $ (fromRight [[]] $ pTriangle 2)) @?= "1 1 1"     

        , testCase "depth 3" $
            (toRowMajor $ (fromRight [[]] $ pTriangle 3)) @?= "1 1 1 1 2 1"   

        , testCase "depth 6" $
            (toRowMajor $ (fromRight [[]] $ pTriangle 6)) @?= "1 1 1 1 2 1 1 3 3 1 1 4 6 4 1 1 5 10 10 5 1"                
        ]  
    ]