module Employment where

import Base
import MultiDay
import Prelude hiding (when, and, or, until, Right)

----------------------------------------------------------------
--- EMPLOYMENT CONTRACTS

--- Definite Employment contracts
definiteEmploymentContract :: String -> String -> Date -> Integer -> Double -> Labour -> Currency -> Contract
definiteEmploymentContract p1 p2 t n lx lk k = multiWorkWeek n p1 p2 t lx (Right (Employ (Labour lk))) `and` give (multiMonth (round $ (fromInteger n)/30) p1 p2 (t +. 30) costOfLabour (Currency k))
    where   costOfLabour = 30 * lx * convert t (Labour lk) / convert t (Currency k)

definiteEmploymentContractIO :: Date -- end of month
                             -> Integer -- contract length in days
                             -> Double -- daily hours of work
                             -> Labour -- type of work
                             -> Currency -- payment currency
                             -> ContractIO
definiteEmploymentContractIO t n lx lk k = multiWorkWeekIO n t lx (Right (Employ (Labour lk))) `andIO` giveIO (multiMonthIO (round $ (fromInteger n)/30+0.5) (t +. 30) costOfLabour (Currency k))
    where   costOfLabour = 30 * lx * convert t (Labour lk) / convert t (Currency k)
---

--- Indefinite Employment contracts
indefiniteEmploymentContract :: String -> String -> Date -> Double -> Labour -> Currency -> Contract
indefiniteEmploymentContract p1 p2 t lx lk k = definiteEmploymentContract p1 p2 t (now -.. t) lx lk k

indefiniteEmploymentContractIO :: Date -> Double -> Labour -> Currency -> ContractIO
indefiniteEmploymentContractIO t lx lk k = do
    n <- date %-.. konst t
    definiteEmploymentContractIO t (n+1) lx lk k
---
----------------------------------------------------------------