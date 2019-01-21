module PTriangle
    ( pTriangle
    , toRowMajor
    )
    where    

import Data.List (foldl')
import Safe (atDef)

type PTriangle = [[ Int ]] -- one-based


pTriangle :: Int -> Either String PTriangle
pTriangle depth = 
    case depth < 1 of
        True ->
            Left $ "Depth must be >= 1"
        False ->
            let
                f :: PTriangle -> Int -> PTriangle 
                f acc n = 
                    let
                        rowLen = n -- equilateral triangle
                    
                        formula :: Int -> Int
                        formula k = 
                            case (k == 1) || (k == rowLen) of
                                True ->
                                    1

                                False ->
                                    let 
                                        priorRow = atDef [] acc $ n - 2
                                        topLeft  = atDef 0 priorRow $ k - 2
                                        topRight = atDef 0 priorRow $ k - 1
                                    in
                                        topLeft + topRight                    
                    in
                        acc ++ [map formula [1 .. rowLen]]                  
            in
                Right $ foldl' f [] [1 .. depth]


toRowMajor :: PTriangle -> String
toRowMajor xss =
    unwords $ map show $ concat xss