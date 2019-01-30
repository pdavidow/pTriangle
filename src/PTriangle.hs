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
                    -- Since triangle is equilateral, n is both depth and row length
                    let
                        newRow = map formula [1 .. n] 

                        formula :: Int -> Int
                        formula k = 
                            if (k == 1) || (k == n) then
                                1
                            else
                                let 
                                    priorRow = head acc 
                                    topLeft  = atDef 0 priorRow $ k - 2
                                    topRight = atDef 0 priorRow $ k - 1
                                in
                                    topLeft + topRight 
                    in
                        newRow : acc               
            in
                Right $ reverse $ foldl' f [] [1 .. depth]   


toRowMajor :: PTriangle -> String
toRowMajor xss =
    unwords $ map show $ concat xss