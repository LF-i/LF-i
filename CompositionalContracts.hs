import Data.Time
import Prelude hiding (when, and, or, until, Right)
import Data.Fixed
import System.Random
import Control.Monad hiding (when)
import System.IO.Unsafe
import Data.List hiding (and, or)

--------------------------------------------------------------------
--- HELPER FUNCTIONS
import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

roundForDefect :: Double -> Integer
roundForDefect n = if n > 0.5 then round (n - 0.5) else round n

string2double :: String -> Double
string2double n = fromInteger (read n)

string2int :: String -> Int
string2int n = fromInteger (read n)

string2integer :: String -> Integer
string2integer n = fromInteger (read n)

sortByDate :: Contract -> Contract
sortByDate []     = []
sortByDate (c:cs) = sortByDate [ a | a <- cs, executionDate a <= executionDate c] ++ [c] ++ sortByDate [a | a <- cs, executionDate a > executionDate c]

sortByDateIO :: ContractIO -> ContractIO
sortByDateIO c = do
    c <- c
    return $ sortByDateIO' c
    where 
        sortByDateIO' :: [Transaction2] -> [Transaction2]
        sortByDateIO' []     = []
        sortByDateIO' (c:cs) = sortByDateIO' [ a | a <- cs, executionDate2 a <= executionDate2 c] ++ [c] ++ sortByDateIO' [a | a <- cs, executionDate2 a > executionDate2 c]

sumByGroup :: [(String, Double)] -> [(String, Double)]
sumByGroup = map (foo . unzip) . groupBy (\x y -> fst x == fst y) . sort            
   where foo (names, vals) = (head names, sum vals)

-- IMPORTANT: to be used on Something == Ton contracts only
sumByHolderAndCrop :: Contract -> Contract
sumByHolderAndCrop = map foo . groupBy (\x y -> holder x == holder y && counterparty x == counterparty y && (growWhat . right $ something x) == (growWhat . right $ something y))
   where 
       foo (c:cs) = Transaction {  holder = holder c, 
                            counterparty = counterparty c,
                            amount = foo' (c:cs),
                            something = something c, 
                            executionDate = last . sort $ map executionDate (c:cs) }
       foo' :: Contract -> Double
       foo' [] = 0
       foo' (c:cs) = amount c + foo' cs

uniq :: Eq b => [b] -> [b]
uniq = map head . group
--------------------------------------------------------------------

--------------------------------------------------------------------
--- TIME
type Date = UTCTime
date = lift2 UTCTime (lift dateToDay getCurrentTime) 0
now = UTCTime (dateToDay $ unsafePerformIO getCurrentTime) 0 :: Date -- for utility purposes, to be changed to: mkDate today's date

mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

mkDate :: (Integer, Int, Int) -> Date
mkDate (year,month,day) = dayToDate (fromGregorian year month day)

addDiffDates :: Date -> Date -> Date
addDiffDates t1 t2 = addToDate (diffDates t1 t2) t2

addToDate :: Integer -> Date -> Date
addToDate n date = dayToDate (addDays n $ dateToDay date)

dayToDate :: Day -> Date
dayToDate day = UTCTime day 0

dateToDay :: Date -> Day
dateToDay t = read $ formatTime defaultTimeLocale "%Y-%m-%d" t

diffDates :: Date -> Date -> Integer
diffDates t1 t2 = - diffDays t1' t2'
    where t1' = dateToDay t1
          t2' = dateToDay t2
--------------------------------------------------------------------

--------------------------------------------------------------------
--- BUILD DATATYPES

data Action =   Grow { growWhere :: Something, growWhat :: Crop } | 
                Raise { raiseWhere :: Something, raiseWhat :: Animal } |
                Own { actionSomething :: Something } |
                Employ { employLabour :: Something } deriving (Eq, Read, Show)

data Currency = CHF | EUR | GBP | USD deriving (Eq, Read, Show)
currencies = [CHF,EUR,GBP,USD]

data Crop = Wheat | Oat | Barley | Soybeans | Corn deriving (Eq, Read, Show)
tonsOfCrops = [(Crop Wheat),(Crop Oat),(Crop Barley),(Crop Soybeans),(Crop Corn)]

data Animal = Cattle | Sheep | Poultry deriving (Eq, Read, Show)

data Labour = Worker | JuniorLawyer | SeniorLawyer | JuniorConsultant | SeniorConsultant deriving (Eq, Read, Show)

data Material = Pipe | Wood deriving (Eq, Read, Show)

data Something =    Currency { currency :: Currency }                               | 
                    Crop { crop :: Crop }                                           |
                    Good                                                            |
                    Material { material :: Material }                               |    
                    Service                                                         | 
                    Ton { tonOfSomething :: Something }                             | 
                    Right { right :: Action }                                       | 
                    Land { landLocation :: Location, landSize :: Size }             |
                    Animal { animal :: Animal }                                     |
                    Labour { labour :: Labour }
                    deriving (Eq, Read, Show)

type Location = String
type Size = Double -- size in acres

type Counterparty = String
type Holder = String

data Transaction = Transaction { holder :: Holder,
                   counterparty :: Counterparty,
                   amount :: Double,
                   something :: Something,
                   executionDate :: Date} deriving (Show, Eq, Read)

type Contract = [Transaction]

data Transaction2 = Transaction2 {  amount2 :: Double,
                                    something2 :: Something,
                                    executionDate2 :: Date} deriving (Show, Eq, Read)

type ContractIO = IO [Transaction2]

type Owner = String

data Deposit = Deposit {    owner   :: Owner,
                            eur     :: Double,
                            chf     :: Double,
                            usd     :: Double,
                            gbp     :: Double,
                            good    :: Double,
                            service :: Double,
                            contract :: Contract    } deriving (Show, Eq, Read)

type Deposits = [Deposit]

data Deposit3 = Deposit3 {  eurIO     :: Double,
                            chfIO     :: Double,
                            usdIO     :: Double,
                            gbpIO     :: Double,
                            goodIO    :: Double,
                            serviceIO :: Double,
                            contractIO :: [Transaction2] } deriving (Show, Eq, Read)

type DepositIO = IO Deposit3

--------------------------------------------------------------------

----------------------------------------------------------------
--- DEPOSIT MANAGEMENT
initiateDeposit :: String -> Double -> Double -> Double -> Double -> Double -> Double -> Contract -> Deposit
initiateDeposit owner eur chf usd gbp good service contract = Deposit owner eur chf usd gbp good service contract

initiateDepositIO :: Double -> Double -> Double -> Double -> Double -> Double -> ContractIO -> IO Deposit3
initiateDepositIO eur chf usd gbp good service contract = do
    contract <- contract
    return $ Deposit3 eur chf usd gbp good service contract

findDeposit :: Owner -> Deposits -> Deposit
findDeposit p ds = head $ filter (\ds -> p == owner ds) $ ds

removeDeposit :: [String] -> Deposits -> Deposits
removeDeposit []             ds = ds
removeDeposit (p:ps) ds = removeDeposit ps $ filter (\ds -> p /= owner ds) $ ds
----------------------------------------------------------------

-- e.g.
d1 = Deposit "LF" 1000 1000 1000 1000 10 5000 zero
d2 = Deposit "PZ" 1000 1000 1000 1000 10 5000 zero
d3 = Deposit "OW" 1000 1000 1000 1000 10 5000 zero
d4 = [d3, d2, d1]
--

----------------------------------------------------------------
--- BASE COMBINATORS
zero :: Contract
zero = []

zeroIO :: ContractIO
zeroIO = return []

one :: String -> String -> Something -> Contract
one counterparty holder k = Transaction holder counterparty 1 k now : []

oneIO :: Something -> ContractIO
oneIO k = do
    date <- date
    return $ [Transaction2 1 k date]

give :: Contract -> Contract
give [] = []
give (c:cs) = Transaction { holder = counterparty c,
                      counterparty = holder c,
                      amount = amount c, 
                      something = something c,
                      executionDate = executionDate c } : [] `and` give cs

giveIO :: ContractIO -> ContractIO
giveIO c = do
    c <- c
    return $ give c
    where 
        give :: [Transaction2] -> [Transaction2]
        give [] = []
        give (c:cs) = Transaction2 { amount2 = - amount2 c,
                              something2 = something2 c,
                              executionDate2 = executionDate2 c } : [] ++ give cs

and :: Contract -> Contract -> Contract
and c1 c2 = c1 ++ c2

andIO :: ContractIO -> ContractIO -> ContractIO
andIO c1 c2 = lift2 (++) c1 c2

or :: String -> Contract -> Contract -> Contract
or p c1 c2
    | valueS p c1 >= valueS p c2 = c1
    | otherwise                  = c2

orIO :: ContractIO -> ContractIO -> ContractIO
orIO c1 c2 = do
    putStrLn "Do you want to acquire contract A or contract B (A/B)?"
    choice <- getLine
    condIO (return $ choice == "A") c1 c2

whenT :: (Date -> Date -> Bool) -> Date -> Date -> Contract -> Contract
whenT f t1 t2 cs = adjustDate f t1 t2 cs "not deferred"

whenIO :: IO Bool -> ContractIO -> ContractIO
whenIO bool c = do
    bool <- bool
    if bool then c else zeroIO

scale :: Double -> Contract -> Contract
scale _ []     = []
scale x (c:cs) = Transaction { holder = holder c,
                         counterparty = counterparty c,
                         amount = x * amount c,
                         something = something c,
                         executionDate = executionDate c } : [] ++ scale x cs

scaleIO :: IO Double -> ContractIO -> ContractIO
scaleIO x cs = do
    b <- lift length cs %== return 0
    if b then cs else do
    x <- x
    (c:cs) <- cs
    (return [ Transaction2 { amount2 = x * amount2 c,
                    something2 = something2 c,
                    executionDate2 = executionDate2 c } ]) `andIO` condIO (return $ length cs /= 0) (scaleIO (return x) (return cs)) zeroIO

-- e.g.
c1 = one "LF" "PZ" (Currency USD)
c2 = one "PZ" "LF" (Currency CHF)
c3 = and c1 c2

--

zcb :: String -> String -> Date -> Double -> Something -> Contract
zcb p1 p2 t x k = whenT at t now $ scale x (one p1 p2 k)

zcbIO :: Date -> IO Double -> Something -> ContractIO
zcbIO t x k = whenIO (atIO t date) $ scaleIO x (oneIO k)

-- e.g.
t1 = mkDate (2022,03,30)
t2 = mkDate (2022,06,31)

c4 = zcb "LF" "PZ" t1 5 (Currency CHF)

a1 = and (zcb "LF" "PZ" t1 100 (Currency USD)) (zcb "PZ" "LF" t2 100 (Currency EUR))
--

anytimeT :: String -> (Date -> Date -> Bool) -> Date -> Date -> Contract -> Contract
anytimeT _ _  _  _ [] = []
anytimeT p f t1 t2 cs = or p c zero
              where c = adjustDate f t1 t2 cs "deferred"

anytimeIO :: IO Bool -> ContractIO -> ContractIO
anytimeIO bool cs = condIO bool (orIO cs zeroIO) zeroIO

cond :: Bool -> Contract -> Contract -> Contract -- Bool should be simulated
cond bool c1 c2 = if bool then c1 else c2

condIO :: IO Bool -> ContractIO -> ContractIO -> ContractIO -- Bool should be simulated
condIO bool c1 c2 = do
    bool <- bool
    if bool then c1 else c2

until :: Bool -- simulate this
      -> Contract -> Contract
until bool c = if bool then zero else c

untilIO :: IO Bool -> ContractIO -> ContractIO
untilIO bool c = do
    bool <- bool
    if bool then zeroIO else c

european :: String -> Date -> Contract -> Contract
european p t c = whenT at t now $ or p c zero

europeanIO :: Date -> ContractIO -> ContractIO
europeanIO t c = whenIO (atIO t date) $ orIO c zeroIO

american :: String -> Date -> Date -> Contract -> Contract
american p t1 t2 c = anytimeT p between t1 t2 c

americanIO :: Date -> Date -> ContractIO -> ContractIO 
americanIO t1 t2 c = anytimeIO (betweenIO t1 t2) c

----------------------------------------------------------------
--- VALUE MANAGEMENT
valueMultipleNP :: Contract -> Double
valueMultipleNP [] = 0
valueMultipleNP (c:cs) = value c + valueMultipleNP cs

value :: Transaction -> Double
value c = (amount c) * (convert $ something c) / (1.0001 ** (fromInteger $ diffDates now $ executionDate c))

expectedLoss = 0.05 -- 3000




valueS :: String -> Contract -> Double
valueS p cs =  sum [ value a | a <- cs, holder a == p, (check2 $ something a) /= True ] - 
                    sum [ value a | a <- cs, counterparty a == p, (check2 $ something a) /= True ] -
                    sum [ (1 - expectedLoss) * value a | a <- cs, counterparty a == p, (check2 $ something a) == True ] +
                    sum [ (1 - expectedLoss) * value a | a <- cs, holder a == p, (check2 $ something a) == True ] +
                    (harvestRevenue $ sumByHolderAndCrop [ a | a <- cs, holder a == p, (check1 $ something a) == True ])
                        where
                            check1 :: Something -> Bool
                            check1 (Right (Grow _ _)) = True
                            check1 _ = False
                            check2 :: Something -> Bool
                            check2 (Ton (Crop _)) = True
                            check2 _ = False
                            harvestRevenue :: Contract -> Double
                            harvestRevenue [] = 0
                            harvestRevenue (c:cs) = ((ypha tck * q * (1 - l) * p) / dr) + harvestRevenue cs
                                where
                                    q = landSize . growWhere . right $ something c
                                    dr = 1.0001 ** (fromInteger . diffDates now . addToDate 1 $ executionDate c)
                                    p = convert . Ton $ Crop tck -- expected Crop price per Ton
                                    tck = growWhat . right $ something c -- grown Crop
                                    l = expectedLoss
                                    -- exp = cropQuantity -- * 0.5 -- measurement cost: $0.5/Crop

harvestRevenue :: Contract -> Double
harvestRevenue c = harvestRevenue' c
    where
        harvestRevenue' :: [Transaction] -> Double
        harvestRevenue' [] = 0
        harvestRevenue' (c:cs) = ((sYPA * q * (1 - l) * p) / dr) + harvestRevenue' cs
            where
                q = landSize . growWhere . right $ something c
                dr = 1.0001 ** (fromInteger . diffDates now . addToDate 1 $ executionDate c)
                sYPA = ypha tck -- average yield per harvested acre
                p = convert . Ton $ Crop tck -- expected Crop price per Ton
                tck = growWhat . right $ something c -- grown Crop
                l = expectedLoss

valueM :: Contract -> [(String,Double)]
valueM c =   valueM' [] $ 
                    [ a | a <- c, (check $ something a) == False ] 
                    ++ 
                    sumByHolderAndCrop [ a | a <- c, (check $ something a) == True ]
                    where
                        check :: Something -> Bool
                        check (Right (Grow _ _)) = True
                        check _ = False
                        valueM' :: [(String,Double)] -> Contract -> [(String,Double)]
                        valueM' ws  []    = sumByGroup ws
                        valueM' [] (c:cs) = valueM' [(holder c, valueHolder c), (counterparty c, valueCounterparty c)] cs
                        valueM' ws (c:cs) = valueM' (ws ++ [(holder c, valueHolder c), (counterparty c, valueCounterparty c)]) cs
                        valueHolder :: Transaction -> Double
                        valueHolder c = valueS (holder c) [c]
                        valueCounterparty :: Transaction -> Double
                        valueCounterparty c = valueS (counterparty c) [c]


convert :: Something -> Double
convert (Currency USD) = 1
convert (Currency EUR) = 1.2
convert (Currency CHF) = 1.1
convert (Currency GBP) = 1.4
convert Good = 500
convert (Material Pipe) = 2500
convert Service = 25 -- "minimum wage"
convert (Ton (Crop Wheat)) = 200 -- TO CHANGE
convert (Ton (Crop Oat)) = 200 -- TO CHANGE
convert (Ton (Crop Barley)) = 200 -- TO CHANGE
convert (Ton (Crop Soybeans)) = 200 -- TO CHANGE
convert (Ton (Crop Corn)) = 200 -- TO CHANGE
convert (Right (Grow _ Wheat)) = 0
convert (Right (Grow _ Oat)) = 0
convert (Right (Grow _ Barley)) = 0
convert (Right (Grow _ Soybeans)) = 0
convert (Right (Grow _ Corn)) = 0
convert (Labour Worker) = 20
convert (Labour JuniorLawyer) = 50
convert (Labour SeniorLawyer) = 100
convert (Labour JuniorConsultant) = 50
convert (Labour SeniorConsultant) = 100
convert _ = 0

-- Seasonal Yield Per Harvested Acre
ypha :: Crop -> Double
ypha ck
    | ck == Wheat = 2.3
    | ck == Soybeans = 1.27
    | ck == Oat = 0.99
    | ck == Barley = 1.24
    | ck == Corn = 4.14
    | otherwise = error $ show ck ++ " is not a crop."


convertIO :: Something -> IO Double
convertIO k = return $ convert k
----------------------------------------------------------------

----------------------------------------------------------------
--- TIME MANAGEMENT
-- Are we at t?
at :: Date -> Date -> Bool
at t now = now == t

atIO :: Date -> IO Date -> IO Bool
atIO t now = lift2 (==) now (return t)

-- Are we before t?
before :: Date -> Date -> Bool
before t now = now < t

-- Are we after t?
after :: Date -> Date -> Bool
after t now = t < now

afterIO :: Date -> IO Date -> IO Bool
afterIO t now = return t %< now

beforeIO :: Date -> IO Date -> IO Bool
beforeIO t now = now %< return t

-- Are we between t1 and t2
between :: Date -> Date -> Bool
between t1 t2 = t1 <= now && now <= t2

-- Are we between t1 and t2
betweenIO :: Date -> Date -> IO Bool
betweenIO t1 t2 = (afterIO t1 date %|| atIO t1 date) %&& (beforeIO t2 date %|| atIO t2 date)

adjustDate :: (Date -> Date -> Bool) -> Date -> Date -> Contract -> String -> Contract
adjustDate f t1 t2 c t
    | (f1 == at1) && (f2 == at2) && (f3 == at3) && (f4 == at4) = changeDate t1 c
    | (f1 == between1) && (f2 == between2) && (f3 == between3) = changeDate (if t == "deferred" then t2 else t1) c -- deferred execution (always last moment)
    | (f1 == before1) && (f2 == before2) && (f3 == before3)    = changeDate (if t == "deferred" then t1 else t2) c -- deferred execution (always last moment)
    | (f1 == after1) && (f2 == after2) && (f3 == after3)       = changeDate (if after t1 t2 then t2 else t1) c -- now if True, earliest time horizon if False 
    | otherwise                                                = c
    where f1       = f t1 t2
          f2       = f t1 (addDiffDates now t2) -- at vs. between
          f3       = f now now -- before & after vs. at and between
          f4       = f t2 t1
          at1      = at t1 t2
          at2      = at t1 (addDiffDates now t2)
          at3      = at now now
          at4      = at t2 t1
          between1 = between t1 t2
          between2 = between t1 (addDiffDates now t2)
          between3 = between now now
          before1  = before t1 t2
          before2  = before t1 (addDiffDates now t2)
          before3  = before now now
          after1   = after t1 t2
          after2   = after t1 (addDiffDates now t2)
          after3   = after now now

changeDate :: Date -> Contract -> Contract
changeDate _ []     = []
changeDate t (c:cs) = Transaction { holder = holder c, 
                            counterparty = counterparty c, 
                            amount = amount c, 
                            something = something c, 
                            executionDate = t } : [] ++ changeDate t cs

changeDateIO :: IO Date -> ContractIO -> ContractIO
changeDateIO t c = do
    t <- t
    c <- c
    return $ changeDate' t c
    where 
        changeDate' :: Date -> [Transaction2] -> [Transaction2]
        changeDate' _ []     = []
        changeDate' t (c:cs) = [Transaction2 { amount2 = amount2 c, 
                                        something2 = something2 c, 
                                        executionDate2 = t }] ++ changeDate' t cs
----------------------------------------------------------------

----------------------------------------------------------------
--- UTILITY
lift :: (a -> b) -> IO a -> IO b
lift f x = do
    x <- x
    return $ f x

lift2 :: (a -> b -> c) -> IO a -> IO b -> IO c
lift2 f x y = f <$> x <*> y

lift3 :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
lift3 f x y z = f <$> x <*> y <*> z

instance (Num a) => Num (IO a) where
    fromInteger i = return $ fromInteger i
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum

instance Num UTCTime where
    fromInteger i = error "This is not an Integer."
    (+) a b | a >= b = addToDate (diffDays a' b') a | otherwise = addToDate (diffDays b' a') b
        where a' = dateToDay a
              b' = dateToDay b
    (-) a b = error "Use (-..) for subtracting dates."
    (*) a b = error "Cannot multiply dates."
    abs i = error "Cannot use abs on dates."
    signum i = error "Cannot use signum on dates."

instance (Fractional a) => Fractional (IO a) where
    fromRational i = return $ fromRational i
    (/) = lift2 (/)

(%==),(%/=),(%<),(%<=),(%=),(%>=),(%>) :: (Ord a) => IO a -> IO a -> IO Bool
(%==) = lift2 (==)
(%/=) = lift2 (/=)
(%<) = lift2 (<)
(%<=) = lift2 (<=)
(%=) = lift2 (==)
(%>=) = lift2 (>=)
(%>) = lift2 (>) 

(%&&),(%||) :: IO Bool -> IO Bool -> IO Bool
(%&&) = lift2 (&&)
(%||) = lift2 (||)

(+.),(-.) :: Date -> Integer -> Date
(+.) t n = addToDate n t
(-.) t n = addToDate (-n) t

(%+.),(%-.) :: IO Date -> IO Integer -> IO Date
(%+.) t n = lift2 addToDate n t
(%-.) t n = lift2 addToDate (-n) t

(-..) :: Date -> Date -> Integer
(-..) t1 t2 = diffDates t2 t1
----------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------
myDeposit = initiateDepositIO 1000 1000 1000 1000 10 5000 zeroIO

--- ORDERS

-- ideally orders would appear as pop-ups in the front-end of the trading software
-- this is just  a proof-of-concept
-- wrap order combinators around the deposit - they allow combining contracts and deposits

market :: IO Deposit3 -> ContractIO -> IO Deposit3
market d c = do
    putStrLn "How many units of the underlying do you want to market (0-∞)?"
    n <- getLine
    let c = copyIO (string2integer n) c
    addContractIO d c

--- Limit Order
-- The order can be executed at this price or at one more favourable to the investor
limitOrder :: IO Deposit3 -> IO Bool -> ContractIO -> IO Deposit3
limitOrder d bool c = anytimeD bool (market d c) d

-- e.g.
o1 = limitOrder myDeposit (return 10 %> return 5) (oneIO (Currency USD))
--

anytimeD :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a
anytimeD bool d1 d2 = condD bool (orD d1 d2) d2

condD :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a -- Bool should be simulated
condD bool c1 c2 = do
    bool <- bool
    if bool then c1 else c2

orD :: (Show a, Eq a, Read a) => IO a -> IO a -> IO a
orD d1 d2 = do
    putStrLn "Would you like to pursue the order (Y/N)?"
    choice <- getLine
    if choice == "Y" then d1 else d2
---

--- Stop Order
-- The order is executed at the best available price once the price reaches a particular price or a less-favourable price
stopOrder :: IO Deposit3 -> IO Bool -> ContractIO -> IO Deposit3
stopOrder d bool c = whenD bool (market d c) d

whenD :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a
whenD bool d1 d2 = condD bool d1 d2
---

--- Stop-Limit Order
-- The order becomes a limit order as soon as the market price is equal or less favourable than the stop price
stopLimitOrder :: IO Deposit3 -> IO Bool -> IO Bool -> ContractIO -> IO Deposit3
stopLimitOrder d bool1 bool2 c = stopOrder d' bool1 c
    where d' = limitOrder d bool2 c
---

--- Market-if-Touched (MIT) Order
-- The order is executed at the best available price after a trade occurs at a specified price or at a price more favourable than the specified price.
-- Like a stop order, but ensuring profits are taken rather than limiting a loss.
marketIfTouchedOrder :: IO Deposit3 -> IO Bool -> ContractIO -> IO Deposit3
marketIfTouchedOrder d bool c = stopOrder d bool c
---

--- Day Order
-- Expires at the end of the trading day (as any order unless otherwise stated)
dayOrder :: IO Deposit3 -> Date -> ContractIO -> IO Deposit3
dayOrder d today c = anytimeD (atIO today date) (market d c) d
---

--- Time-of-Day Order
-- Specifies a particular period of time during the day when the order can be executed
timeOfDay :: IO Deposit3 -> (Date,Date) -> ContractIO -> IO Deposit3
timeOfDay d (t1,t2) c = anytimeD (betweenIO t1 t2) (market d c) d


--- Fill-or-Kill Order
-- Must be executed immediately on receipt or not at all
fillOrKillOrder :: IO Deposit3 -> ContractIO -> IO Deposit3
fillOrKillOrder d c = orD (market d c) d

----------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

----- AGRICULTURAL CONTRACTS -----

----------------------------------------------------------------
--- CASH RENT CONTRACTS

cashrentIO :: Integer -- duration in days
            -> Date -- start Date
            -> Something -- the rented plot of land
            -> Double -- price of Land per duration of the contract
            -> Currency -- payment Currency (payment arrives day after duration end)
            -> Crop -- crop to be Grown
            -> ContractIO
cashrentIO n t l p k ck = multiDayIO n t 1 (Right (Grow l ck)) `andIO` zcbIO (t +. n) (return p) (Currency k)

cashrent :: Integer -> Counterparty -> Owner -> Date -> Something -> Double -> Currency -- (payment arrives day after duration end)
         -> Crop -> Contract
cashrent n farmer landowner t l p k ck = multiDay n landowner farmer t 1 (Right (Grow l ck)) `and` zcb farmer landowner (t +. n) p (Currency k)

-- e.g.
plot1 = Land "l1" 100

cr1 = cashrent 260 "Farmer" "Landowner" t2 plot1 20000 USD Wheat

-- linear functions
revenuePerFarmer :: Contract -> Double
revenuePerFarmer c = valueS "Farmer" $ [ a | a <- c, b <- tonsOfCrops, something a == Ton b ]

expensePerFarmer :: Contract -> Double
expensePerFarmer c = valueS "Farmer" $ [ a | a <- c, b <- tonsOfCrops, something a /= Ton b ]
----------------------------------------------------------------

----------------------------------------------------------------
--- CROPSHARE CONTRACTS
-- The farmer bears the measurement expenses. This is a ccounted for in "valueS".

cropshareIO :: Integer -- duration
            -> Date -- start Date
            -> Something -- the rented plot of land
            -> Double -- yield share to owner
            -> IO Double -- harvest yield of crop ck at time t in Tons
            -> Crop -- Crop type (payment arrives day after duration end)
            -> ContractIO
cropshareIO n t l s y ck
    | s > 1 || s < 0    = error "Share to owner must be in the range 0-1."
    | otherwise         =  multiDayIO n t 1 (Right (Grow l ck)) `andIO` giveIO (zcbIO (t +. n) (return s * y) (Ton (Crop ck)))

cropshare :: Integer -> Counterparty -> Owner -> Date -> Something -> Double -> Crop -> Contract
cropshare n farmer landowner t l s ck
    | s > 1 || s < 0    = error "Share to owner must be in the range 0-1."
    | otherwise         = multiDay n landowner farmer t 1 (Right (Grow l ck)) `and` zcb farmer landowner (t +. n) (s * y) (Ton (Crop ck))
                    where y = ypha ck * landSize l -- yield

-- e.g.
cr2 = cropshare 260 "Farmer" "Landowner" t2 plot1 0.5 Wheat

cr2' = cr2 `and` (addCosts expMeasurementExpenses USD cr2)

expMeasurementExpenses :: Double
expMeasurementExpenses = 1000

addCosts :: Double -> Currency -> Contract -> Contract
addCosts x k c = zcb "Farmer" "Nature" (executionDate . last $ sortByDate cr2) x (Currency k)
----------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------
--- INDEFINITE TIME CONTRACTS (to be implemented in IO)
indefiniteDuration :: Integer -> Contract -> Contract
indefiniteDuration noticeInDays (c:cs)
    | noticeInDays >= 30  = if after ((executionDate c) -. noticeInDays) now then indefiniteDuration noticeInDays $ changeDate (now +. oneMonth) (c:cs) else (c:cs)
    | otherwise           = (c:cs)
           where oneMonth = 30 :: Integer

indefiniteDurationIO :: Integer -> ContractIO -> ContractIO
indefiniteDurationIO noticeInDays c = do
    (c:cs) <- c
    b <- afterIO (executionDate2 c -. noticeInDays) date
    if noticeInDays >= 30 && b then indefiniteDurationIO noticeInDays $ changeDateIO (date %+. oneMonth) (return $ c:cs) else (return $ c:cs)
        where oneMonth = return 30 :: IO Integer

terminateindefiniteContractIO :: ContractIO -> ContractIO
terminateindefiniteContractIO c = do
    c <- c
    terminateindefiniteContractIO' c
    where 
        terminateindefiniteContractIO' :: [Transaction2] -> ContractIO
        terminateindefiniteContractIO' []     = zeroIO
        terminateindefiniteContractIO' (c:cs) = condIO (afterIO lastDate date) zeroIO (return [c]) `andIO` terminateindefiniteContractIO' cs
            where lastDate = executionDate2 c
    

----------------------------------------------------------------

----------------------------------------------------------------
--- PROCUREMENT CONTRACTS

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
        economicPriceAdjustment list vc fc = 1 + (actualVC list / (return vc) - 1) * return (vc / (vc + fc))
        actualVC :: [(String,IO Double,IO Double)] -> IO Double
        actualVC []                           = 0
        actualVC ((item,x,p):rest) = x*p + actualVC rest

--- Fixed-price contracts with (prospective) price redetermination
-- Extend the contract and potentially change its price
fixPriceProspectiveRed :: Integer -- how long should the next period of performance be?
                       -> Double -- by what magnitude should the payment be scaled? (in the IO form this is asked to the holder)
                       -> Double -- ceiling (e.g. 2.0)
                       -> Contract -- original contract: i.e. payment + goods/services transfer
                       -> Contract
fixPriceProspectiveRed n x max c =  c 
                                    `and`
                                    (scale (min x max) $ whenT at (t +. n) now c1) 
                                    `and` -- only scale payment contracts
                                    (whenT at (t +. n) now c2)
                    where   t       = executionDate . last $ sortByDate c -- payment same time as last good/service transfer
                            c1      = [ a | a <- c, (check $ something a) == True ]
                            c2      = [ a | a <- c, b <- c1, a /= b ]
                            check :: Something -> Bool
                            check (Currency _)  = True
                            check _             = False


fixPriceProspectiveRedIO :: Integer -> IO Double -> Double -> ContractIO -> ContractIO
fixPriceProspectiveRedIO n x max c = do
    b <- lift length c %== return 0
    if b then c else do
    c' <- c
    let t = lift (executionDate2 . last) $ sortByDateIO c
        c1 = return [ a | a <- c', (check $ something2 a) == True ]
        c2 = return [ a | a <- c', (check $ something2 a) == False ]
        t' = t %+. (return n)
    t <- t
    t' <- t'
    whenIO (atIO t date) $ c 
        `andIO`
        (scaleIO (lift2 min x (return max)) $ whenIO (atIO t' date) c1)
        `andIO`
        (whenIO (atIO t' date) c2)
        where   
            check :: Something -> Bool
            check (Currency _)  = True
            check _             = False

-- e.g.
proc1 = fixPriceProspectiveRed 10 1.5 2.0 $ (one "PZ" "LF" (Currency EUR)) `and` (one "LF" "PZ" Good)
proc1' = fixPriceProspectiveRedIO 10 (return 1.5) 2.0 $ (oneIO (Currency EUR)) `andIO` (oneIO Good)
proc1'' = fixPriceProspectiveRedIO 10 (return 1.5) 2.0 $ (zcbIO t1 10 (Currency EUR)) `andIO` (zcbIO t1 10 Good)
---

--- Fixed-ceiling-price contracts with retroactive price redetermination
fixCeilingPriceRetroactiveRed :: Double -- this should be Obs Double (in the IO form this is asked to the holder)
                              -> Double -- fixed ceiling
                              -> Contract -- original contract: payment + goods/services transfer
                              -> Contract
fixCeilingPriceRetroactiveRed x max c = whenT at t now $ scale (min x max) c1 `and` c2
                                where   t  = executionDate $ last $ sortByDate c -- payment same time as last good/service transfer
                                        c1 = [ a | a <- c, (check $ something a) == True ]
                                        c2 = [ a | a <- c, (check $ something a) == False ]
                                        check :: Something -> Bool
                                        check (Currency _)  = True
                                        check _             = False

fixCeilingPriceRetroactiveRedIO :: IO Double -> Double -> ContractIO -> ContractIO
fixCeilingPriceRetroactiveRedIO x max c = do
    b <- lift length c %== return 0
    if b then c else do
    c' <- c
    let t = lift (executionDate2 . last) $ sortByDateIO c
        c1 = return [ a | a <- c', (check $ something2 a) == True ]
        c2 = return [ a | a <- c', (check $ something2 a) == False ]
    t <- t
    whenIO (atIO t date) $ scaleIO (lift2 min x (return max)) c1 `andIO` c2
        where   
            check :: Something -> Bool
            check (Currency _)  = True
            check _             = False

-- e.g.
proc2 = fixCeilingPriceRetroactiveRed vcWeightedChange 1.1 $ whenT at t2 now $ (one "PZ" "LF" (Currency EUR)) `and` (one "LF" "PZ" Good)

vcWeightedChange :: Double
vcWeightedChange = (sum $ take 1000 $ randomRs (0.8,1.3) (mkStdGen 0)) / 1000

proc2' = fixCeilingPriceRetroactiveRedIO 1.05 1.1 $ (oneIO (Currency EUR)) `andIO` (oneIO Good)
proc2'' = fixCeilingPriceRetroactiveRedIO 1.05 1.1 $ whenIO (atIO t2 date) $ (oneIO (Currency EUR)) `andIO` (oneIO Good)
---

--- Firm-fixed-price, level-of-effort term contracts
-- specific level of effort to be specified for Good/Service. Price does not change.
-- Implementation would not differ from normal contract
---



-------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    condIO (return $ max > q) (return cs) (scaleIO (return $ max / q) (return cs))

setMin :: Double -> Something -> Contract -> Contract
setMin min k cs = cond (min < q) cs (scale (q / min) cs)
    where q = sum [ amount a | a <- cs, something a == k ]

setMinIO :: Double -> Something -> ContractIO -> ContractIO
setMinIO min k cs = do
    cs <- cs
    let q = sum [ amount2 a | a <- cs, something2 a == k ]
    condIO (return $ min < q) (return cs) (scaleIO (return $ q / min) (return cs))
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- COMBINATORS LV1
varPrice :: Purchaser -> Supplier -> Date -> Double -> Currency -> Contract
varPrice p1 p2 t p k = zcb p1 p2 t p (Currency k)
varPriceIO :: Date -> IO Double -> Currency -> ContractIO
varPriceIO t p k = zcbIO t p (Currency k)
 
fixPrice :: Purchaser -> Supplier -> Date -> Double -> Currency -> Contract
fixPrice p1 p2 t p k = zcb p1 p2 t p (Currency k)
fixPriceIO :: Date -> Double -> Currency -> ContractIO
fixPriceIO t p k = zcbIO t (return p) (Currency k)

--- COMBINATORS LV2
awardFeeDiscr :: String -> String -> Date -> [(Integer,Double)] -> Integer -> Currency -> Contract
awardFeeDiscr p1 p2 t list effortLV k = fixPrice p1 p2 t fee k
    where fee = snd $ head [a | a <- list, fst a == effortLV ]
awardFeeDiscrIO :: Date -> [(Integer,Double)] -> IO Integer -> Currency -> ContractIO
awardFeeDiscrIO t list effortLV k = do
    effortLV <- effortLV
    let fee = snd $ head [a | a <- list, fst a == effortLV ]
    fixPriceIO t fee k


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
costPlusFixFeeIOContract :: Date -> IO Bool -> IO Double -> Double -> Currency -> ContractIO
costPlusFixFeeIOContract t b x f k = costContractIO t x k `andIO` condIO b (fixPriceIO t f k) zeroIO
-- Completion form: fee is released if, upon completion, actual cost <= estimated cost
-- Term form: fee is released if, upon completion, level of effort has been satisfactory (actualLV >= minimumLV)


--- Fixed-price-incentive (firm/successive target) contracts
-- if finalPrice < max, price == final, else price == ceiling
fixPriceIncentiveTargetContract :: String -> String -> Date -> Double -> Double -> Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> Contract
fixPriceIncentiveTargetContract p1 p2 t targProfit finalCost targCost f max k = setMax max (Currency k) (fixPrice p1 p2 t finalCost k `and` varPrice p1 p2 t (f targProfit finalCost targCost) k)
fixPriceIncentiveFirmTargetContractIO :: Date -> Double -> IO Double -> Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> ContractIO
fixPriceIncentiveFirmTargetContractIO t targProfit finalCost targCost f max k = do
    finalCost <- finalCost
    setMaxIO max (Currency k) $ fixPriceIO t finalCost k `andIO` varPriceIO t (return $ f targProfit finalCost targCost) k
fixPriceIncentiveSuccTargetContractIO :: Date -> IO Double -> IO Double -> IO Double -> (Double -> Double -> Double -> Double) -> Double -> Currency -> ContractIO
fixPriceIncentiveSuccTargetContractIO t succTargProfit finalCost succTargCost f max k = do
    finalCost <- finalCost
    succTargProfit <- succTargProfit
    succTargCost <- succTargCost
    setMaxIO max (Currency k) $ fixPriceIO t finalCost k `andIO` varPriceIO t (return $ f succTargProfit finalCost succTargCost) k


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
costPlusIncentiveFeeContractIO t targetFee targCost finalCost f minFee maxFee k = costContractIO t finalCost k `andIO` (setMinIO minFee (Currency k) $ setMaxIO maxFee (Currency k) (varPriceIO t (lift3 f (return targetFee) finalCost (return targCost)) k))


--- Cost-plus-award-fee contracts
costPlusAwardFee :: String -> String -> Date -> Double -> Bool -> Double -> Double -> Currency -> Contract
costPlusAwardFee p1 p2 t base b award finalCost k = costContract p1 p2 t finalCost k `and` fixPrice p1 p2 t base k `and` cond b (fixPrice p1 p2 t award k) zero


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- MANDATES


mandate :: Supplier -> Purchaser -> Date -> Something -> Contract
mandate p1 p2 t k = zcb p1 p2 t 1 k
mandateIO :: Date -> Something -> ContractIO
mandateIO t k = zcbIO t (return 1) k

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- SUPPLY CONTRACTS


--- Definite Quantity Contracts
definiteQuantity :: Integer -> Supplier -> Purchaser -> Date -> Double -> Something -> Double -> Currency -> Contract
definiteQuantity days p1 p2 t x1 something x2 k = multiDay days p1 p2 t x1 something `and` give (multiDay days p2 p1 t x2 (Currency k))
definiteQuantityIO :: Integer -> Date -> IO Double -> Something -> IO Double -> Currency -> ContractIO
definiteQuantityIO days t x1 k1 x2 k2 = do
    x1 <- x1
    x2 <- x2
    multiDayIO days t x1 k1 `andIO` giveIO (multiDayIO days t x2 (Currency k2))


--- Requirement contracts ("spot market": order for the same day)
requirementContract :: String -> String -> (Date, Date) -> Double -> Something -> Double -> Currency -> Double -> Contract
requirementContract p1 p2 (t1,t2) x1 k1 x2 k2 max = cond (max - x1 >= 0)
    (definiteQuantity 1 p1 p2 z x1 k1 x2 k2 `and` requirementContract p1 p2 (z,t2) x1 k1 x2 k2 (max-x1))
    zero
    where z = t1 +. (round $ x1 * (fromInteger $ t2 -.. t1) / max)
requirementContractIO :: (Date, Date) -> IO Double -> Something -> IO Double -> Currency -> Double -> ContractIO -- In IO form, max limit is mandatory (infinite loop otherwise)
requirementContractIO (_ ,_ ) _  _  _  _  0   = zeroIO
requirementContractIO (t1,t2) x1 k1 x2 k2 max = condIO ((return max - x1) %< return 0) zeroIO (anytimeIO (betweenIO t1 t2) $ do
    date <- date %+. 7
    max' <- return max - x1
    definiteQuantityIO 1 date x1 k1 x2 k2 `andIO` requirementContractIO (t1,t2) x1 k1 x2 k2 max') -- (date %+. 1)


--- Time-and-materials contracts
timeAndMaterialsContract :: String -> String -> Date -> Double -> Material -> Double -> Labour -> Currency -> Contract
timeAndMaterialsContract p1 p2 t mx mk lx lk k = definiteQuantity 1 p1 p2 t mx (Material mk) costOfMaterials k `and` labourHourContract p1 p2 t lx lk k
    where   costOfMaterials   = mx * convert (Material mk) / convert (Currency k)
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
    where   costOfLabour = lx * convert (Labour lk) / convert (Currency k)
labourHourContractIO :: Date -> IO Double -> Labour -> Currency -> ContractIO
labourHourContractIO t lx lk k = definiteQuantityIO 1 t lx (Right (Employ (Labour lk))) costOfLabour k
    where   costOfLabour = lx * convertIO (Labour lk) / convertIO (Currency k)
---

--- Definite Employment contracts
definiteEmploymentContract :: String -> String -> Date -> Integer -> Double -> Labour -> Currency -> Contract
definiteEmploymentContract p1 p2 t n lx lk k = multiDay n p1 p2 t lx (Right (Employ (Labour lk))) `and` give (multiMonth (round $ (fromInteger n)/30) p1 p2 (t +. 30) costOfLabour (Currency k))
    where   costOfLabour = 30 * lx * convert (Labour lk) / convert (Currency k)
definiteEmploymentContractIO :: Date -- end of month
                             -> Integer -- contract length in days
                             -> Double -- daily hours of work
                             -> Labour -- type of work
                             -> Currency -- payment currency
                             -> ContractIO
definiteEmploymentContractIO t n lx lk k = multiDayIO n t lx (Right (Employ (Labour lk))) `andIO` giveIO (multiMonthIO (round $ (fromInteger n)/30+0.5) (t +. 30) costOfLabour (Currency k))
    where   costOfLabour = 30 * lx * convert (Labour lk) / convert (Currency k)
---

--- Indefinite Employment contracts

-- indefiniteEmploymentContract :: String -> String -> Date -> Double -> Labour -> Currency -> Contract
-- indefiniteEmploymentContract p1 p2 t lx lk k = definiteEmploymentContract p1 p2 t (now -.. t) lx lk k

-- indefiniteEmploymentContractIO :: Date -> Double -> Labour -> Currency -> ContractIO
-- indefiniteEmploymentContractIO t lx lk k = definiteEmploymentContractIO t (date -.. t) lx lk k
---

----------------------------------------------------------------
--- INDEFINITE-QUANTITY CONTRACTS

-- ASSUMPTION (symulation only): the max amount is ordered through individual orders uniformally distributed across time and quantity
indefiniteContract :: String -> String -> (Date,Date) -> Something -> Currency -> (Double,Double) -> Integer -> Integer -> Contract
indefiniteContract _  _  (_ ,_ ) _  _  (_   ,_   ) 0        _ = zero
indefiniteContract p1 p2 (t1,t2) k1 k2 (min1,max1) optionsN n = definiteQuantity 1 p1 p2 time x1 k1 x2 k2 `and` indefiniteContract p1 p2 (time,newDeadline) k1 k2 (min1, max1 - x1) (optionsN - 1) n
    where x1 = max1 / fromInteger optionsN
          x2 = (x1 * convert k1) / convert (Currency k2)
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
    (definiteQuantityIO 1 date (return x1) k1 (return x2) k2) `andIO` (indefiniteContractIO (t1, t2 +. n) k1 k2 (max 0 (min1 - x1), max 0 (max1 - x1)) (optionsN - 1) n)
-- Options:
-- 1) Wrap setMax (setMin) around definiteQuantity to set maximum (minimum) order per limit

----------------------------------------------------------------


----------------------------------------------------------------
--- RENTAL CONTRACTS (or, multi-day contracts)

multiDay :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiDay 0 _  _  _ _ _ = zero
multiDay n p1 p2 t x k = zcb p1 p2 t x k `and` multiDay (n-1) p1 p2 (t +. 1) x k
multiDayIO :: Integer -> Date -> Double -> Something -> ContractIO
multiDayIO 0 _ _ _ = zeroIO
multiDayIO n t x k = zcbIO t (return x) k `andIO` multiDayIO (n-1) (t +. 1) x k

multiWorkWeek :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiWorkWeek 0 _  _  _ _ _ = zero
multiWorkWeek n p1 p2 t x k = multiDay 5 p1 p2 t x k `and` multiWorkWeek (n-1) p1 p2 (t +. 7) x k
multiWorkWeekIO :: Integer -> Date -> Double -> Something -> ContractIO
multiWorkWeekIO 0 _ _ _ = zeroIO
multiWorkWeekIO n t x k = multiDayIO 5 t x k `andIO` multiWorkWeekIO (n-1) (t +. 7) x k

multiWeek :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiWeek 0 _  _  _ _ _ = zero
multiWeek n p1 p2 t x k = zcb p1 p2 t x k ++ multiWeek (n-1) p1 p2 (t +. 7) x k
multiWeekIO :: Integer -> Date -> Double -> Something -> ContractIO
multiWeekIO 0 _ _ _ = zeroIO
multiWeekIO n t x k = zcbIO t (return x) k `andIO` multiWeekIO (n-1) (t +. 7) x k

multiMonth :: Integer -> String -> String -> Date -> Double -> Something -> Contract
multiMonth 0 _  _  _ _ _ = zero
multiMonth n p1 p2 t x k = zcb p1 p2 t x k `and` multiMonth (n-1) p1 p2 (t +. 30) x k
multiMonthIO :: Integer -> Date -> Double -> Something -> ContractIO
multiMonthIO 0 _ _ _ = zeroIO
multiMonthIO n t x k = zcbIO t (return x) k `andIO` multiMonthIO (n-1) (t +. 30) x k

extendMultiDay :: Integer -> Contract -> Contract
extendMultiDay n cs = cs `and` multiDay n (counterparty c) (holder c) (executionDate c +. 1) (amount c) (something c)
    where c = last $ sortByDate cs
----------------------------------------------------------------

----------------------------------------------------------------
--- EXTEND CONTRACT (General, only good for underlying contracts with unique date)

-- Unique payment
extend :: Double -- how much should the payment increase?
       -> Date -- to what date should the execution be delayed?
       -> Contract -> Contract
extend x t c = scale x $ changeDate t c

-- Renew
renew :: Date -> Contract -> Contract
renew t c = c `and` changeDate t c

-- Copy
copy :: Integer -> Contract -> Contract
copy 0 c = []
copy n c = c `and` copy (n-1) c

copyIO :: Integer -> ContractIO -> ContractIO
copyIO 0 c = zeroIO
copyIO n c = c `andIO` copyIO (n-1) c
----------------------------------------------------------------


----------------------------------------------------------------
--- MONTE CARLO ON BOOL (not used)
symulateBool :: IO Bool -> Double
symulateBool bool = (unsafePerformIO $ symulateBool_ bool 10000) / 10000
    where 
        symulateBool_ :: IO Bool -> Double -> IO Double
        symulateBool_ bool 0 = return 0
        symulateBool_ bool i = lift (fromIntegral . fromEnum) bool + symulateBool_ bool (i-1)
----------------------------------------------------------------

--- E.g.
rainInCyprus :: IO Double -- Obs Double
rainInCyprus = do
    sec <- sec
    let (x:xs) = randomRs (0,15) (mkStdGen sec)
    return x

sec :: IO Int
sec = do
    now <- getCurrentTime
    return $ read (formatTime defaultTimeLocale "%q" now)
---

--------------------------------------------------------------------------------------------------------------------------------
--- STRESS TESTING

aggregate :: Something -> Contract -> Double -- possibility of more contracts, although impossible
aggregate k []     = 0
aggregate k (c:cs) = if something c == k then amount c + aggregate k cs else aggregate k cs


--------------------------------------------------------------------------------------------------------------------------------

symulate :: Deposit -> Deposit
symulate d = aggregateAll d . sortByDate $ [ a | a <- contract d, owner d == holder a ] ++ give [ a | a <- contract d, owner d == counterparty a ]
    where
        aggregateAll :: Deposit -> Contract -> Deposit
        aggregateAll d [] = d
        aggregateAll d (c:cs) 
            |   eur d     + aggregate (Currency EUR) c < 0  ||
                chf d     + aggregate (Currency CHF) c < 0  ||
                (usd d    + aggregate (Currency USD) c) < 0 ||
                gbp d     + aggregate (Currency GBP) c < 0  ||
                good d    + aggregate Good c < 0            ||
                service d + aggregate Service c < 0 = error $ owner d ++ "'s deposit cannot fulfill contract " ++ show c
            | otherwise = aggregateAll Deposit {
                            owner = owner d,
                            eur = eur d + aggregate (Currency EUR) c,
                            chf = chf d + aggregate (Currency CHF) c,
                            usd = usd d + aggregate (Currency USD) c,
                            gbp = gbp d + aggregate (Currency GBP) c,
                            good = good d + aggregate Good c,
                            service = service d + aggregate Service c,
                            contract = contract d
                            } cs
        give :: Contract -> Contract
        give [] = []
        give (c:cs) = Transaction { holder = holder c,
                            counterparty = counterparty c,
                            amount = - amount c,
                            something = something c,
                            executionDate = executionDate c } : [] `and` give cs
        aggregate :: Something -> Transaction -> Double
        aggregate k c = if something c == k then amount c else 0

addAndActivateIO :: IO Deposit3 -> ContractIO -> IO Deposit3
addAndActivateIO d cs = do
    d <- d
    cs <- cs
    return $ fromToIO' d cs
    where 
        fromToIO' :: Deposit3 -> [Transaction2] -> Deposit3
        fromToIO' d [] = d
        fromToIO' d (c:cs)
            | eurIO d + aggregate (Currency EUR) c < 0   || 
                chfIO d + aggregate (Currency CHF) c < 0   ||
                usdIO d + aggregate (Currency USD) c < 0   ||
                gbpIO d + aggregate (Currency GBP) c < 0   ||
                goodIO d + aggregate Good c < 0 ||
                serviceIO d + aggregate Service c < 0 = error $ "The deposit cannot fulfill contract " ++ show c
            | otherwise = fromToIO' 
                            Deposit3 {
                            eurIO = eurIO d + aggregate (Currency EUR) c,
                            chfIO = chfIO d + aggregate (Currency CHF) c,
                            usdIO = usdIO d + aggregate (Currency USD) c,
                            gbpIO = gbpIO d + aggregate (Currency GBP) c,
                            goodIO = goodIO d + aggregate Good c,
                            serviceIO = serviceIO d + aggregate Service c,
                            contractIO = contractIO d
                            } cs
        aggregate :: Something -> Transaction2 -> Double
        aggregate k c = if something2 c == k then amount2 c else 0




activateIO'' :: IO Deposit3 -> IO Deposit3
activateIO'' d = do
    d <- d
    let cs = contractIO d
    return $ fromToIO' d cs
    where 
        fromToIO' :: Deposit3 -> [Transaction2] -> Deposit3
        fromToIO' d [] = d
        fromToIO' d (c:cs)
            |   eurIO d + aggregate (Currency EUR) c < 0   || 
                chfIO d + aggregate (Currency CHF) c < 0   ||
                usdIO d + aggregate (Currency USD) c < 0   ||
                gbpIO d + aggregate (Currency GBP) c < 0   ||
                goodIO d + aggregate Good c < 0            ||
                serviceIO d + aggregate Service c < 0 = error $ "The deposit cannot fulfill contract " ++ show c
            | otherwise = fromToIO' 
                            Deposit3 {
                            eurIO = eurIO d + aggregate (Currency EUR) c,
                            chfIO = chfIO d + aggregate (Currency CHF) c,
                            usdIO = usdIO d + aggregate (Currency USD) c,
                            gbpIO = gbpIO d + aggregate (Currency GBP) c,
                            goodIO = goodIO d + aggregate Good c,
                            serviceIO = serviceIO d + aggregate Service c,
                            contractIO = contractIO d
                            } cs
        aggregate :: Something -> Transaction2 -> Double
        aggregate k c = if something2 c == k then amount2 c else 0

cIO4 = zcbIO (mkDate (2022,04,13)) (return 500) (Currency USD)

addContract :: Deposit -> Contract -> Deposit
addContract d c = Deposit { owner = owner d,
                            eur = eur d,
                            chf = chf d,
                            usd = usd d,
                            gbp = gbp d,
                            good = good d,
                            service = service d,
                            contract = contract d ++ c }

addContractIO :: IO Deposit3 -> ContractIO -> IO Deposit3
addContractIO d c = do
    d <- d
    c <- c
    return Deposit3 {   eurIO = eurIO d,
                        chfIO = chfIO d,
                        usdIO = usdIO d,
                        gbpIO = gbpIO d,
                        goodIO = goodIO d,
                        serviceIO = serviceIO d,
                        contractIO = contractIO d ++ c }

removeContractIO :: IO Deposit3 -> Int -> ContractIO -> IO Deposit3
removeContractIO d n c = do
    d <- d
    c <- c
    let tot = length [ a | a <- contractIO d,  a == head c ]
    if tot - n >= 0 then return Deposit3 {  eurIO = eurIO d,
                                            chfIO = chfIO d,
                                            usdIO = usdIO d,
                                            gbpIO = gbpIO d,
                                            goodIO = goodIO d,
                                            serviceIO = serviceIO d,
                                            contractIO = [ a | a <- contractIO d,  a == head c ] ++ take (tot-n) [ a | a <- contractIO d,  a == head c ] }
    else error $ "Contract " ++ show c ++ " is not in deposit " ++ show d ++ "."

dIO1 = initiateDepositIO 1000 1000 1000 1000 10 5000 zeroIO
cIO1 = (zcbIO now (return 500) (Currency USD)) `andIO` (giveIO $ zcbIO now (return 1) Good)
cIO2 = (zcbIO t1 (return 500) (Currency USD)) `andIO` (giveIO $ zcbIO t2 (return 1) Good)
cIO3 = (zcbIO now (return 7500) (Currency USD)) `andIO` (giveIO $ zcbIO now (return 15) Good)

----------------------------------------------------------------
