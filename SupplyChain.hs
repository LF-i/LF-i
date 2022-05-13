module SupplyChain where

import Base
import MultiDay
import Prelude hiding (when, and, or, until, Right)

----------------------------------------------------------------
--- UTILITY FUNCTIONS
type Supplier = String
type Purchaser = String

setMax :: Double -> Something -> Contract -> Contract
setMax max k cs = cond (max > q) cs (scale (max / q) cs)
    where q = sum [ amount a | a <- cs, something a == k ]

setMaxIO :: Double -> Something -> ContractIO -> ContractIO
setMaxIO max k cs = do
    cs <- cs
    let q = sum [ amount2 a | a <- cs, something2 a == k ]
    condIO (return $ max > q) (return cs) (scaleIO (konst $ max / q) (return cs))

setMin :: Double -> Something -> Contract -> Contract
setMin min k cs = cond (min < q) cs (scale (q / min) cs)
    where q = sum [ amount a | a <- cs, something a == k ]

setMinIO :: Double -> Something -> ContractIO -> ContractIO
setMinIO min k cs = do
    cs <- cs
    let q = sum [ amount2 a | a <- cs, something2 a == k ]
    condIO (return $ min < q) (return cs) (scaleIO (konst $ q / min) (return cs))
----------------------------------------------------------------

----------------------------------------------------------------
--- COMBINATORS LV1
varPrice :: Purchaser -> Supplier -> Date -> Double -> Currency -> Contract
varPrice p1 p2 t p k = zcb p1 p2 t p (Currency k)

varPriceIO :: Date -> IO Double -> Currency -> ContractIO
varPriceIO t p k = zcbIO t p (Currency k)
 
fixPrice :: Purchaser -> Supplier -> Date -> Double -> Currency -> Contract
fixPrice p1 p2 t p k = zcb p1 p2 t p (Currency k)

fixPriceIO :: Date -> Double -> Currency -> ContractIO
fixPriceIO t p k = zcbIO t (konst p) (Currency k)

--- COMBINATORS LV2
awardFeeDiscr :: String -> String -> Date -> [(Integer,Double)] -> Integer -> Currency -> Contract
awardFeeDiscr p1 p2 t list effortLV k = fixPrice p1 p2 t fee k
    where fee = snd $ head [a | a <- list, fst a == effortLV ]

awardFeeDiscrIO :: Date -> [(Integer,Double)] -> IO Integer -> Currency -> ContractIO
awardFeeDiscrIO t list effortLV k = do
    effortLV <- effortLV
    let fee = snd $ head [a | a <- list, fst a == effortLV ]
    fixPriceIO t fee k
----------------------------------------------------------------

----------------------------------------------------------------
--- PAYMENT METHODS

--- Cost contracts
costContract :: Purchaser -> Supplier -> Date -> Double -> Currency -> Contract
costContract p1 p2 t p k = varPrice p1 p2 t p k

costContractIO :: Date -> IO Double -> Currency -> ContractIO
costContractIO t p k = varPriceIO t p k

--- Cost-sharing contracts
-- same as cost contracts, but take below combinator as Double
costSharingContract :: Purchaser -> Supplier -> Date -> Double -> Double -> Currency -> Contract
costSharingContract p1 p2 t s p k = costContract p1 p2 t (s * p) k

--- Cost-plus-fixed-fee contracts
-- fixed fee (f) depending on condition
costPlusFixFeeContract :: Purchaser -> Supplier -> Date -> Bool -> Double -> Double -> Currency -> Contract
costPlusFixFeeContract p1 p2 t b f x k = costContract p1 p2 t x k `and` cond b (fixPrice p1 p2 t f k) zero

costPlusFixFeeContractIO :: Date -> IO Bool -> IO Double -> Double -> Currency -> ContractIO
costPlusFixFeeContractIO t b x f k = costContractIO t x k `andIO` condIO b (fixPriceIO t f k) zeroIO
-- Completion form: fee is released if, upon completion, actual cost <= estimated cost
-- Term form: fee is released if, upon completion, level of effort has been satisfactory (actualLV >= minimumLV)

--- Fixed-price-incentive (firm/successive target) contracts
-- if finalPrice < max, price == final, else price == ceiling
fixPriceIncentiveTargetContract :: String -> String -> Date -> Double -> Double -> Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> Contract
fixPriceIncentiveTargetContract p1 p2 t targProfit finalCost targCost f max k = setMax max (Currency k) (fixPrice p1 p2 t finalCost k `and` varPrice p1 p2 t (f targProfit finalCost targCost) k)
fixPriceIncentiveFirmTargetContractIO :: Date -> Double -> IO Double -> Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> ContractIO
fixPriceIncentiveFirmTargetContractIO t targProfit finalCost targCost f max k = do
    finalCost <- finalCost
    setMaxIO max (Currency k) $ fixPriceIO t finalCost k `andIO` varPriceIO t (konst $ f targProfit finalCost targCost) k

fixPriceIncentiveSuccTargetContractIO :: Date -> IO Double -> IO Double -> IO Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> ContractIO
fixPriceIncentiveSuccTargetContractIO t succTargProfit finalCost succTargCost f max k = do
    finalCost <- finalCost
    succTargProfit <- succTargProfit
    succTargCost <- succTargCost
    setMaxIO max (Currency k) $ fixPriceIO t finalCost k `andIO` varPriceIO t (konst $ f succTargProfit finalCost succTargCost) k

--- Fixed-price contracts with award fees
type EffortLV = Integer
type Fee = Double
fixPriceAwardFeesContract :: String -> String -> Date -> [(EffortLV,Fee)] -> EffortLV -> Double -> Currency -> Contract
fixPriceAwardFeesContract p1 p2 t list effortLV p k = fixPrice p1 p2 t p k `and` awardFeeDiscr p1 p2 t list effortLV k

fixPriceAwardFeesContractIO :: Date -> [(EffortLV,Fee)] -> IO EffortLV -> Double -> Currency -> ContractIO
fixPriceAwardFeesContractIO t list effortLV p k = fixPriceIO t p k `andIO` awardFeeDiscrIO t list effortLV k

--- Cost-plus-incentive-fee contracts
costPlusIncentiveFeeContract :: String -> String -> Date -> Double -> Double -> Double -> (Double -> Double -> Double -> Double) -> Double -> Double -> Currency -> Contract
costPlusIncentiveFeeContract p1 p2 t targetFee targCost finalCost f minFee maxFee k = costContract p1 p2 t finalCost k `and` (setMin minFee (Currency k) $ setMax maxFee (Currency k) (varPrice p1 p2 t (f targetFee finalCost targCost) k))

costPlusIncentiveFeeContractIO :: Date -> Double -> Double -> IO Double -> (Double -> Double -> Double -> Double) -> Double -> Double -> Currency -> ContractIO
costPlusIncentiveFeeContractIO t targetFee targCost finalCost f minFee maxFee k = costContractIO t finalCost k `andIO` (setMinIO minFee (Currency k) $ setMaxIO maxFee (Currency k) (varPriceIO t (lift3 f (konst targetFee) finalCost (konst targCost)) k))

--- Cost-plus-award-fee contracts
costPlusAwardFee :: String -> String -> Date -> Double -> Bool -> Double -> Double -> Currency -> Contract
costPlusAwardFee p1 p2 t base b award finalCost k = costContract p1 p2 t finalCost k `and` fixPrice p1 p2 t base k `and` cond b (fixPrice p1 p2 t award k) zero
----------------------------------------------------------------

----------------------------------------------------------------
--- MANDATES

mandate :: Supplier -> Purchaser -> Date -> Something -> Contract
mandate p1 p2 t (Right k) = zcb p1 p2 t 1 (Right k)
mandate _  _  _ _         = error "Rights only."

mandateIO :: Date -> Something -> ContractIO
mandateIO t (Right k) = zcbIO t (konst 1) (Right k)
mandateIO _ _         = error "Rights only."
----------------------------------------------------------------

----------------------------------------------------------------
--- SUPPLY CONTRACTS

--- Definite Quantity Contracts
definiteQuantity :: Integer -> Supplier -> Purchaser -> Date -> Double -> Something -> Double -> Currency -> Contract
definiteQuantity days p1 p2 t x1 something x2 k = multiDay days p1 p2 t x1 something `and` give (multiDay days p2 p1 t x2 (Currency k))

definiteQuantityIO :: Integer -> Date -> IO Double -> Something -> IO Double -> Currency -> ContractIO
definiteQuantityIO days t x1 k1 x2 k2 = do
    x1 <- x1
    x2 <- x2
    multiDayIO days t x1 k1 `andIO` giveIO (multiDayIO days t x2 (Currency k2))
---

--- Requirement contracts ("spot market": order for the same day)
requirementContract :: String -> String -> (Date, Date) -> Double -> Something -> Double -> Currency -> Double -> Contract
requirementContract p1 p2 (t1,t2) x1 k1 x2 k2 max = cond (max - x1 >= 0)
    (definiteQuantity 1 p1 p2 z x1 k1 x2 k2 `and` requirementContract p1 p2 (z,t2) x1 k1 x2 k2 (max-x1))
    zero
    where z = t1 +. (round $ x1 * (fromInteger $ t2 -.. t1) / max)

requirementContractIO :: (Date, Date) -> IO Double -> Something -> IO Double -> Currency -> Double -> ContractIO -- In IO form, max limit is mandatory (infinite loop otherwise)
requirementContractIO (_ ,_ ) _  _  _  _  0   = zeroIO
requirementContractIO (t1,t2) x1 k1 x2 k2 max = condIO ((konst max - x1) %< konst 0) zeroIO (anytimeIO (betweenIO t1 t2) $ do
    date <- date %+. 7
    max' <- konst max - x1
    definiteQuantityIO 1 date x1 k1 x2 k2 `andIO` requirementContractIO (t1,t2) x1 k1 x2 k2 max') -- (date %+. 1)
---

--- Time-and-materials contracts
timeAndMaterialsContract :: String -> String -> Date -> Double -> Material -> Double -> Labour -> Currency -> Contract
timeAndMaterialsContract p1 p2 t mx mk lx lk k = definiteQuantity 1 p1 p2 t mx (Material mk) costOfMaterials k `and` labourHourContract p1 p2 t lx lk k
    where   costOfMaterials   = mx * convert t (Material mk) / convert t (Currency k)

timeAndMaterialsContractIO :: Date -> IO Double -> Material -> IO Double -> Labour -> Currency -> ContractIO 
timeAndMaterialsContractIO t mx mk lx lk k = definiteQuantityIO 1 t mx (Material mk) costOfMaterials k `andIO` labourHourContractIO t lx lk k
    where   costOfMaterials = mx * convertIO (Material mk) / convertIO (Currency k)
---

--- Time-and-materials contracts (with ceiling, passive)
timeAndMaterialsCeilingContract :: String -> String -> Date -> Double -> Material -> Double -> Labour -> Currency -> Double -> Contract 
timeAndMaterialsCeilingContract p1 p2 t mx mk lx lk k max = setMax max (Currency k) $ timeAndMaterialsContract p1 p2 t mx mk lx lk k

timeAndMaterialsCeilingContractIO :: Date -> IO Double -> Material -> IO Double -> Labour -> Currency -> Double -> ContractIO
timeAndMaterialsCeilingContractIO t mx mk lx lk k max = setMaxIO max (Currency k) $ timeAndMaterialsContractIO t mx mk lx lk k
---

--- Labour-hour contracts
labourHourContract :: String -> String -> Date -> Double -> Labour -> Currency -> Contract
labourHourContract p1 p2 t lx lk k = definiteQuantity 1 p1 p2 t lx (Right (Employ (Labour lk))) costOfLabour k
    where   costOfLabour = lx * convert t (Labour lk) / convert t (Currency k)

labourHourContractIO :: Date -> IO Double -> Labour -> Currency -> ContractIO
labourHourContractIO t lx lk k = definiteQuantityIO 1 t lx (Right (Employ (Labour lk))) costOfLabour k
    where   costOfLabour = lx * convertIO (Labour lk) / convertIO (Currency k)
---
----------------------------------------------------------------

----------------------------------------------------------------
--- INDEFINITE-QUANTITY CONTRACTS

-- ASSUMPTION (symulation only): the max amount is ordered through individual orders uniformally distributed across time and quantity
indefiniteContract :: String -> String -> (Date,Date) -> Something -> Currency -> (Double,Double) -> Integer -> Integer -> Contract
indefiniteContract _  _  (_ ,_ ) _  _  (_   ,_   ) 0        _ = zero
indefiniteContract p1 p2 (t1,t2) k1 k2 (min1,max1) optionsN n = definiteQuantity 1 p1 p2 time x1 k1 x2 k2 `and` indefiniteContract p1 p2 (time,newDeadline) k1 k2 (min1, max1 - x1) (optionsN - 1) n
    where x1 = max1 / fromInteger optionsN
          x2 = (x1 * convert time k1) / convert time (Currency k2)
          timespan = roundForDefect $ (fromInteger $ diffDates t1 t2) / (fromInteger optionsN)
          time = t1 +. timespan
          newDeadline = t2 +. n

-- (WRONG ORDER OF EXECUTION)
indefiniteContractIO :: (Date,Date) -> Something -> Currency -> (Double,Double) -> Integer -> Integer -> ContractIO
indefiniteContractIO (t1,t2) k1 k2 (min1,max1) 0 n = condIO (return $ min1 <= 0) zeroIO (indefiniteContractIO (t1,t2) k1 k2 (min1, max1) 1 n)
indefiniteContractIO (_ ,_ ) _  _  (_   ,0   ) _        _ = zeroIO
indefiniteContractIO (t1,t2) k1 k2 (min1,max1) optionsN n = anytimeIO (betweenIO t1 t2) $ do
    date <- date
    putStrLn $ "How many units of " ++ show k1
    x1' <- getLine
    putStrLn $ "How man units of " ++ show k2
    x2' <- getLine
    let x1 = read x1' :: Double
        x2 = read x2' :: Double
    (definiteQuantityIO 1 date (konst x1) k1 (konst x2) k2) `andIO` (indefiniteContractIO (t1, t2 +. n) k1 k2 (max 0 (min1 - x1), max 0 (max1 - x1)) (optionsN - 1) n)
-- Options:
-- 1) Wrap setMax (setMin) around definiteQuantity to set maximum (minimum) order per limit
----------------------------------------------------------------

----------------------------------------------------------------
--- SPECIAL PROCUREMENT CONTRACTS

--- Fixed-price contracts with (implicit) economic price adjustment
fixPriceEconPriceAdj :: Double -> Contract -- standard payment with estimated price (e.g. zcb "p1" "p2" t1 1000 CHF)
                     -> Contract
fixPriceEconPriceAdj percChange c = scale (1 + percChange) c -- adjust contract value before simulation

--- Fixed-price contracts with (explicit) economic price adjustment
fixPriceEconPriceAdjIO :: Date -- date at which the re-evaluation takes place
                      -> [(String,IO Double,IO Double)] -- prices at contract execution
                      -> Double -> Double -> ContractIO -> ContractIO
fixPriceEconPriceAdjIO t list vc fc c = whenIO (atIO t date) $ scaleIO (economicPriceAdjustment list vc fc) c
    where
        economicPriceAdjustment :: [(String,IO Double,IO Double)] -> Double -> Double -> IO Double
        economicPriceAdjustment list vc fc = 1 + (actualVC list / (konst vc) - 1) * konst (vc / (vc + fc))
        actualVC :: [(String,IO Double,IO Double)] -> IO Double
        actualVC []                           = 0
        actualVC ((item,x,p):rest) = x*p + actualVC rest
---

--- Fixed-price contracts with (prospective) price redetermination
-- Extend the contract and potentially change its price
fixPriceProspectiveRed :: Integer -- how long should the next period of performance be?
                       -> Double -- by what magnitude should the payment be scaled? (in the IO form this is asked to the holder)
                       -> Double -- ceiling (e.g. 2.0)
                       -> Contract -- original contract: i.e. payment + goods/services transfer
                       -> Contract
fixPriceProspectiveRed n x max c =  c 
                                    `and`
                                    (scale (min x max) $ when at (t +. n) now c1) 
                                    `and` -- only scale payment contracts
                                    (when at (t +. n) now c2)
                    where   t       = executionDate . last $ sortByDate c -- payment same time as last good/service transfer
                            c1      = [ a | a <- c, (check $ something a) == True ]
                            c2      = [ a | a <- c, b <- c1, a /= b ]
                            check :: Something -> Bool
                            check (Currency _)  = True
                            check _             = False

fixPriceProspectiveRedIO :: Integer -> IO Double -> Double -> ContractIO -> ContractIO
fixPriceProspectiveRedIO n x max c = do
    b <- lift length c %== konst 0
    if b then c else do
    c' <- c
    let t = lift (executionDate2 . last) $ sortByDateIO c
        c1 = return [ a | a <- c', (check $ something2 a) == True ]
        c2 = return [ a | a <- c', (check $ something2 a) == False ]
        t' = t %+. (konst n)
    t <- t
    t' <- t'
    whenIO (atIO t date) $ c 
        `andIO`
        (scaleIO (lift2 min x (konst max)) $ whenIO (atIO t' date) c1)
        `andIO`
        (whenIO (atIO t' date) c2)
        where   
            check :: Something -> Bool
            check (Currency _)  = True
            check _             = False
---

--- Fixed-ceiling-price contracts with retroactive price redetermination
fixCeilingPriceRetroactiveRed :: Double -- this should be Obs Double (in the IO form this is asked to the holder)
                              -> Double -- fixed ceiling
                              -> Contract -- original contract: payment + goods/services transfer
                              -> Contract
fixCeilingPriceRetroactiveRed x max c = when at t now $ scale (min x max) c1 `and` c2
                                where   t  = executionDate $ last $ sortByDate c -- payment same time as last good/service transfer
                                        c1 = [ a | a <- c, (check $ something a) == True ]
                                        c2 = [ a | a <- c, (check $ something a) == False ]
                                        check :: Something -> Bool
                                        check (Currency _)  = True
                                        check _             = False

fixCeilingPriceRetroactiveRedIO :: IO Double -> Double -> ContractIO -> ContractIO
fixCeilingPriceRetroactiveRedIO x max c = do
    b <- lift length c %== konst 0
    if b then c else do
    c' <- c
    let t = lift (executionDate2 . last) $ sortByDateIO c
        c1 = return [ a | a <- c', (check $ something2 a) == True ]
        c2 = return [ a | a <- c', (check $ something2 a) == False ]
    t <- t
    whenIO (atIO t date) $ scaleIO (lift2 min x (konst max)) c1 `andIO` c2
        where   
            check :: Something -> Bool
            check (Currency _)  = True
            check _             = False
---

--- Firm-fixed-price, level-of-effort term contracts
-- specific level of effort to be specified for Good/Service. Price does not change.
-- Implementation would not differ from normal contract
---

----------------------------------------------------------------