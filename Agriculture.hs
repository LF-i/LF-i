module Agriculture where

import Base
import MultiDay
import Prelude hiding (when, and, or, until, Right)

----------------------------------------------------------------
--- CASH RENT CONTRACTS

cashrentIO :: Integer -- duration in days
            -> Date -- start Date
            -> Something -- the rented plot of land
            -> Double -- price of Land per duration of the contract
            -> Currency -- payment Currency (payment arrives day after rent end)
            -> Crop -- crop to be Grown
            -> ContractIO
cashrentIO n t l p k ck =   multiDayIO n t 1 (Right (Grow l ck)) 
                            `andIO` 
                            zcbIO (t +. n) (konst p) (Currency k)

cashrent :: Integer -> Counterparty -> Owner -> Date -> Something -> Double -> Currency -> Crop -> Contract
cashrent n farmer landowner t l p k ck =    multiDay n landowner farmer t 1 (Right (Grow l ck)) 
                                            `and` 
                                            zcb farmer landowner (t +. n) p (Currency k)

----------------------------------------------------------------

----------------------------------------------------------------
--- CROPSHARE CONTRACTS
-- NB: the farmer bears the measurement expenses

cropshareIO :: Integer -- duration
            -> Date -- start Date
            -> Something -- the rented plot of land
            -> Double -- yield share to owner
            -> IO Double -- harvest yield of crop ck at time t in Tons
            -> Crop -- Crop type (payment arrives day after duration end)
            -> ContractIO
cropshareIO n t l s y ck
    | s > 1 || s < 0    = error "Share to owner must be in the range 0-1."
    | otherwise         =   multiDayIO n t 1 (Right (Grow l ck)) 
                            `andIO` 
                            giveIO (zcbIO (t +. n) (konst s * y) (Ton (Crop ck)))

cropshare :: Integer -> Counterparty -> Owner -> Date -> Something -> Double -> Crop -> Contract
cropshare n farmer landowner t l s ck
    | s > 1 || s < 0    = error "Share to owner must be in the range 0-1."
    | otherwise         =   multiDay n landowner farmer t 1 (Right (Grow l ck)) 
                            `and`
                            zcb farmer landowner (t +. n) (s * ypha ck * landSize l) (Ton (Crop ck))

-- Add measurement & partition expenses to the farmer's contract
-- Wrap around the farmer's contract before feeding it to value
addCosts :: Double -> Currency -> Contract -> Contract
addCosts x k c = zcb "Farmer" "Nature" (lastdate cr2) x (Currency k)
    where   lastdate = \x -> executionDate . last $ sortByDate x

-- e.g. on the use of addCosts
cr2 = cropshare 260 "Farmer" "Landowner" (mkDate (2022,01,01)) (Land "address#1" 10) 0.5 Wheat
cr2' = cr2 `and` (addCosts expMeasurementExpenses USD cr2)

expMeasurementExpenses :: Double
expMeasurementExpenses = 1000
--