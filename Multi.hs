module Multi where

import Base
import Prelude hiding (when, and, or, until, Right)

----------------------------------------------------------------
--- MULTI-DAY/-WEEK/-MONTH CONTRACTS

-- zcb contract starting at date t of the duration of n days
multiDay :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiDay 0 _  _  _ _ _ = zero
multiDay n p1 p2 t x k = zcb p1 p2 t x k `and` multiDay (n-1) p1 p2 (t +. 1) x k

multiDayIO :: Integer -> Date -> Double -> Something -> ContractIO
multiDayIO 0 _ _ _ = zeroIO
multiDayIO n t x k = zcbIO t (konst x) k `andIO` multiDayIO (n-1) (t +. 1) x k

-- zcb contract starting at date t of the duration of n weeks for 5 days per week
multiWorkWeek :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiWorkWeek 0 _  _  _ _ _ = zero
multiWorkWeek n p1 p2 t x k = multiDay 5 p1 p2 t x k `and` multiWorkWeek (n-1) p1 p2 (t +. 7) x k

multiWorkWeekIO :: Integer -> Date -> Double -> Something -> ContractIO
multiWorkWeekIO 0 _ _ _ = zeroIO
multiWorkWeekIO n t x k = multiDayIO 5 t x k `andIO` multiWorkWeekIO (n-1) (t +. 7) x k

-- zcb contract starting at date t of the duration of n weeks for 1 day per week
multiWeek :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiWeek 0 _  _  _ _ _ = zero
multiWeek n p1 p2 t x k = zcb p1 p2 t x k `and` multiWeek (n-1) p1 p2 (t +. 7) x k

multiWeekIO :: Integer -> Date -> Double -> Something -> ContractIO
multiWeekIO 0 _ _ _ = zeroIO
multiWeekIO n t x k = zcbIO t (konst x) k `andIO` multiWeekIO (n-1) (t +. 7) x k

-- zcb contract starting at date t of the duration of n months for 1 day per month
multiMonth :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiMonth 0 _  _  _ _ _ = zero
multiMonth n p1 p2 t x k = zcb p1 p2 t x k `and` multiMonth (n-1) p1 p2 (t +. 30) x k

multiMonthIO :: Integer -> Date -> Double -> Something -> ContractIO
multiMonthIO 0 _ _ _ = zeroIO
multiMonthIO n t x k = zcbIO t (konst x) k `andIO` multiMonthIO (n-1) (t +. 30) x k

-- Extend contracts cs by n days
extendMultiDay :: Integer -> Contract -> Contract
extendMultiDay n cs = cs `and` multiDay n (counterparty c) (holder c) (executionDate c +. 1) (amount c) (something c)
    where c = last $ sortByDate cs
----------------------------------------------------------------