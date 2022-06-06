module Base where

import Data.Time
import Prelude hiding (when, and, or, until, Right)
import Data.Fixed
import Control.Monad hiding (when)
import System.IO.Unsafe
import Data.List hiding (and, or)
import System.IO


--------------------------------------------------------------------
--- GENERAL HELPER FUNCTIONS

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
        sortByDateIO' :: [TransactionIO] -> [TransactionIO]
        sortByDateIO' []     = []
        sortByDateIO' (c:cs) = sortByDateIO' [ a | a <- cs, executionDate2 a <= executionDate2 c] ++ [c] ++ sortByDateIO' [a | a <- cs, executionDate2 a > executionDate2 c]

sumByGroup :: [(String, Double)] -> [(String, Double)]
sumByGroup = map (foo . unzip) . groupBy (\x y -> fst x == fst y) . sort            
   where foo (names, vals) = (head names, sum vals)

-- IMPORTANT: to be used on Something == Ton X contracts only
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
--- TIME HELPER FUNCTIONS

type Date = UTCTime

-- Passive time
-- now = UTCTime (dateToDay $ unsafePerformIO getCurrentTime) 0 :: Date
now = mkDate (2022,01,01)

-- for utility purposes
-- it is suggested to use mkDate (yyyy,mm,dd) with today's date instead

-- Active time
date = lift2 UTCTime (lift dateToDay getCurrentTime) 0

-- Make Date including year, month, days, hours, minutes and seconds
mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

-- Make Date including year, month and days only
mkDate :: (Integer, Int, Int) -> Date
mkDate (year,month,day) = dayToDate (fromGregorian year month day)

-- Add to latest date t2 the difference between the two dates t1 and t2
addDiffDates :: Date -> Date -> Date
addDiffDates t1 t2 = addToDate (diffDates t1 t2) t2

-- Add n days to date t
addToDate :: Integer -> Date -> Date
addToDate n t = dayToDate (addDays n $ dateToDay t)

-- Turn day d to Date format
dayToDate :: Day -> Date
dayToDate d = UTCTime d 0

-- Turn date t to Day format
dateToDay :: Date -> Day
dateToDay t = read $ formatTime defaultTimeLocale "%Y-%m-%d" t

-- Find the difference between date t1 and t2 (t1 < t2)
diffDates :: Date -> Date -> Integer
diffDates t1 t2 = - diffDays t1' t2'
    where t1' = dateToDay t1
          t2' = dateToDay t2
--------------------------------------------------------------------

--------------------------------------------------------------------
--- IO MANAGEMENT

lift :: (a -> b) -> IO a -> IO b
lift f x = do
    x <- x
    return $ f x

lift2 :: (a -> b -> c) -> IO a -> IO b -> IO c
lift2 f x y = f <$> x <*> y

lift3 :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
lift3 f x y z = f <$> x <*> y <*> z

instance (Num a) => Num (IO a) where
    fromInteger i = konst $ fromInteger i
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum

-- Allow summing up two days together
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
    fromRational i = konst $ fromRational i
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

(%-..) :: IO Date -> IO Date -> IO Integer
(%-..) t1 t2 = lift2 diffDates t2 t1
--------------------------------------------------------------------

--------------------------------------------------------------------
--- TRADEABLES, CONTRACTS and PORTFOLIOS

--- Composable Data Types

-- Core combinators
data Action =   Grow { growWhere :: Something, growWhat :: Crop }       | 
                Raise { raiseWhere :: Something, raiseWhat :: Animal }  |
             -- Own { actionSomething :: Something }                    |
                Employ { employLabour :: Something }                    |
                Build { buildSomething :: Something }                   |
                Develop { developSomething :: Something }
                deriving (Eq, Read, Show)
--

-- Core primitives
data Currency = CHF | EUR | GBP | USD deriving (Eq, Read, Show)
currencies = [CHF,EUR,GBP,USD]

data Crop = Wheat | Oat | Barley | Soybeans | Corn deriving (Eq, Read, Show)
tonsOfCrops = [Crop Wheat,Crop Oat,Crop Barley,Crop Soybeans,Crop Corn]

data Animal = Cattle | Sheep | Poultry deriving (Eq, Read, Show)

data Labour = Worker | JuniorLawyer | SeniorLawyer | JuniorConsultant | SeniorConsultant deriving (Eq, Read, Show)

data Material = Pipe | Wood deriving (Eq, Read, Show)

type Location = String

type Size = Double -- size in acres

type Address = String

-- "Something" is a core primitive. 
-- Nonetheless, the function it defines (e.g. currency :: Something -> Currency, crop :: Something -> Crop) are core combinators.
data Something =    Currency { currency :: Currency }                       | 
                    Crop { crop :: Crop }                                   |
                    Good                                                    |
                    Material { material :: Material }                       |    
                    Service                                                 | 
                    Ton { tonOfSomething :: Something }                     | 
                    Right { right :: Action }                               | 
                    Land { landLocation :: Location, landSize :: Size }     |
                    Animal { animal :: Animal }                             |
                    Labour { labour :: Labour }                             |
                    House { houseLocation :: Location }                     |
                    Website { websiteAddress :: Address }
                    deriving (Eq, Read, Show)
---

--- Contracts
type Counterparty = String
type Holder = String

-- Passive transaction (itself a contract) 
data Transaction = Transaction { 
    holder :: Holder,
    counterparty :: Counterparty,
    amount :: Double,
    something :: Something,
    executionDate :: Date } deriving (Show, Eq, Read)

-- Active contract (itself a contract)
data TransactionIO = TransactionIO {  
    amount2 :: Double,
    something2 :: Something,
    executionDate2 :: Date } deriving (Show, Eq, Read)

-- Contracts are made by transactions
type Contract = [Transaction]
type ContractIO = IO [TransactionIO]
---

--- Portfolios
type Owner = String

-- Passive portfolio
data Portfolio = Portfolio {    
    owner   :: Owner,
    eur     :: Double,
    chf     :: Double,
    usd     :: Double,
    gbp     :: Double,
    good    :: Double,
    service :: Double,
    contract :: Contract } deriving (Show, Eq, Read)

type Portfolios = [Portfolio]

-- Active portfolio
data PortfolioIO = PortfolioIO {  
    eurIO     :: Double,
    chfIO     :: Double,
    usdIO     :: Double,
    gbpIO     :: Double,
    goodIO    :: Double,
    serviceIO :: Double,
    contractIO :: [TransactionIO] } deriving (Show, Eq, Read)

konst :: (Num a) => a -> IO a
konst a = return a
--------------------------------------------------------------------

----------------------------------------------------------------
--- TIME BOOLEANS

-- Are we at t?
at :: Date -> Date -> Bool
at t now = now == t

atIO :: Date -> IO Date -> IO Bool
atIO t now = lift2 (==) now (konst t)

-- Are we before t?
before :: Date -> Date -> Bool
before t now = now < t

beforeIO :: Date -> IO Date -> IO Bool
beforeIO t now = now %< konst t

-- Are we after t?
after :: Date -> Date -> Bool
after t now = t < now

afterIO :: Date -> IO Date -> IO Bool
afterIO t now = konst t %< now

-- Are we between t1 and t2 (t1 < t2)?
between :: Date -> Date -> Bool
between t1 t2 = t1 <= now && now <= t2

betweenIO :: Date -> Date -> IO Bool
betweenIO t1 t2 = (afterIO t1 date %|| atIO t1 date) %&& (beforeIO t2 date %|| atIO t2 date)
----------------------------------------------------------------

----------------------------------------------------------------
--- TIME MANAGEMENT

-- For adjusting execution date of transactions wrapped in a timing combinator.
-- The function automatically recognizes the timing combinator fed to it.
adjustDate :: (Date -> Date -> Bool) -> Date -> Date -> Contract -> String -> Contract
adjustDate f t1 t2 c t
    | length c > 1                                             = c
    | (f1 == at1) && (f2 == at2) && (f3 == at3) && (f4 == at4) = changeDate t1 c
    | (f1 == between1) && (f2 == between2) && (f3 == between3) = changeDate (if t == "deferred" then t2 else t1) c -- deferred execution (always last moment)
    | (f1 == before1) && (f2 == before2) && (f3 == before3)    = changeDate (if t == "deferred" then t1 else t2) c -- deferred execution (always last moment)
    | (f1 == after1) && (f2 == after2) && (f3 == after3)       = changeDate (if after t1 t2 then t2 else t1) c -- now if True, earliest time horizon if False 
    | otherwise                                                = c
    where   f1       = f t1 t2
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

-- Moves execution date of all underlying transactions to t
changeDate :: Date -> Contract -> Contract
changeDate _ []     = []
changeDate t (c:cs) = Transaction { 
    holder = holder c, 
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
        changeDate' :: Date -> [TransactionIO] -> [TransactionIO]
        changeDate' _ []     = []
        changeDate' t (c:cs) = [TransactionIO { 
            amount2 = amount2 c, 
            something2 = something2 c, 
            executionDate2 = t }] ++ changeDate' t cs
----------------------------------------------------------------

----------------------------------------------------------------
--- CORE PRIMITIVES

-- Empty contract
zero :: Contract
zero = []

zeroIO :: ContractIO
zeroIO = return []

-- Non-empty contract
one :: String -> String -> Something -> Contract
one counterparty holder k = Transaction holder counterparty 1 k now : []

oneIO :: Something -> ContractIO
oneIO k = do
    date <- date
    return $ [TransactionIO 1 k date]

-- Core primitive for portfolios
addContract :: Portfolio -> Contract -> Portfolio
addContract d c = Portfolio { 
    owner = owner d,
    eur = eur d,
    chf = chf d,
    usd = usd d,
    gbp = gbp d,
    good = good d,
    service = service d,
    contract = contract d ++ c }

addContractIO :: IO PortfolioIO -> ContractIO -> IO PortfolioIO
addContractIO d c = do
    d <- d
    c <- c
    return PortfolioIO {   
        eurIO = eurIO d,
        chfIO = chfIO d,
        usdIO = usdIO d,
        gbpIO = gbpIO d,
        goodIO = goodIO d,
        serviceIO = serviceIO d,
        contractIO = contractIO d ++ c }
----------------------------------------------------------------

----------------------------------------------------------------
--- CORE COMBINATORS

-- Inverts all transactions' rights and obligations
give :: Contract -> Contract
give [] = []
give (c:cs) = Transaction { 
    holder = counterparty c,
    counterparty = holder c,
    amount = amount c, 
    something = something c,
    executionDate = executionDate c } : [] `and` give cs

-- Inverts all transaction amounts' signs
giveIO :: ContractIO -> ContractIO
giveIO c = do
    c <- c
    return $ give c
    where 
        give :: [TransactionIO] -> [TransactionIO]
        give [] = []
        give (c:cs) = TransactionIO { 
            amount2 = - amount2 c,
            something2 = something2 c,
            executionDate2 = executionDate2 c } : [] ++ give cs

-- Joins two contracts together
and :: Contract -> Contract -> Contract
and c1 c2 = c1 ++ c2

andIO :: ContractIO -> ContractIO -> ContractIO
andIO c1 c2 = lift2 (++) c1 c2

-- Decides for the underlying contract with the highest expected value
or :: String -> Contract -> Contract -> Contract
or p c1 c2
    | valueS p c1 >= valueS p c2 = c1
    | otherwise                  = c2

-- Gives the option to decide between the two underlying contracts
orIO :: ContractIO -> ContractIO -> ContractIO
orIO c1 c2 = do
    putStrLn "Do you want to acquire contract A or contract B (A/B)?"
    choice <- getLine
    condIO (return $ choice == "A") c1 c2

--- Timing Combinators
when :: (Date -> Date -> Bool) -> Date -> Date -> Contract -> Contract
when f t1 t2 cs = adjustDate f t1 t2 cs "not deferred" -- non-deferred specifies the date accounting method

whenIO :: IO Bool -> ContractIO -> ContractIO
whenIO bool c = condIO bool c zeroIO

anytime :: String -> (Date -> Date -> Bool) -> Date -> Date -> Contract -> Contract
anytime _ _  _  _ [] = []
anytime p f t1 t2 cs = or p c zero
    where c = adjustDate f t1 t2 cs "deferred" -- deferred specifies the date accounting method

anytimeIO :: IO Bool -> ContractIO -> ContractIO
anytimeIO bool cs = condIO bool (orIO cs zeroIO) zeroIO
---

-- Scales all transaction amounts by x
scale :: Double -> Contract -> Contract
scale _ []     = []
scale x (c:cs) = Transaction { 
    holder = holder c,
    counterparty = counterparty c,
    amount = x * amount c,
    something = something c,
    executionDate = executionDate c } : [] ++ scale x cs

scaleIO :: IO Double -> ContractIO -> ContractIO
scaleIO x cs = do
    b <- lift length cs %== konst 0
    if b then cs else do
    x <- x
    (c:cs) <- cs
    (return [ TransactionIO { 
        amount2 = x * amount2 c,
        something2 = something2 c,
        executionDate2 = executionDate2 c } ]) `andIO` condIO (return $ length cs /= 0) (scaleIO (konst x) (return cs)) zeroIO

cond :: Bool -> Contract -> Contract -> Contract
cond bool c1 c2 = if bool then c1 else c2

condIO :: IO Bool -> ContractIO -> ContractIO -> ContractIO
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
----------------------------------------------------------------

----------------------------------------------------------------
--- CLASSIC COMBINATORS

zcb :: String -> String -> Date -> Double -> Something -> Contract
zcb p1 p2 t x k = when at t now $ scale x (one p1 p2 k)

zcbIO :: Date -> IO Double -> Something -> ContractIO
zcbIO t x k = whenIO (atIO t date) $ scaleIO x (oneIO k)

european :: String -> Date -> Contract -> Contract
european p t c = when at t now $ or p c zero

europeanIO :: Date -> ContractIO -> ContractIO
europeanIO t c = whenIO (atIO t date) $ orIO c zeroIO

american :: String -> Date -> Date -> Contract -> Contract
american p t1 t2 c = anytime p between t1 t2 c

americanIO :: Date -> Date -> ContractIO -> ContractIO 
americanIO t1 t2 c = anytimeIO (betweenIO t1 t2) c
----------------------------------------------------------------

----------------------------------------------------------------
--- VALUE MANAGEMENT

-- Calculates the value of single countract, indifferent on the party
value :: Transaction -> Double
value c = (amount c) * (convert (executionDate c) (something c)) / (1.0001 ** (fromInteger $ diffDates now $ executionDate c))

-- Defines the expected harvest loss for agricultural contracts contracts
expectedLoss :: Double
expectedLoss = 0.05

-- Values contracts c from the perspective of party p
valueS :: String -> Contract -> Double
valueS p c =   sum [ value a | a <- c, holder a == p, (check2 $ something a) /= True ] - 
                sum [ value a | a <- c, counterparty a == p, (check2 $ something a) /= True ] -
                sum [ (1 - expectedLoss) * value a | a <- c, counterparty a == p, (check2 $ something a) == True ] +
                sum [ (1 - expectedLoss) * value a | a <- c, holder a == p, (check2 $ something a) == True ] +
                (harvestRevenue $ sumByHolderAndCrop [ a | a <- c, holder a == p, (check1 $ something a) == True ])
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
                                p = convert (executionDate c) (Ton $ Crop tck) -- expected Crop price per Ton
                                tck = growWhat . right $ something c -- grown Crop
                                l = expectedLoss

-- Calculates the value of contract c from the perspective of all its parties
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

-- Calculates the expected value of all contracts of all parties (no party, NP)
valueMultipleNP :: Contract -> Double
valueMultipleNP [] = 0
valueMultipleNP (c:cs) = value c + valueMultipleNP cs

-- Gets value of Something at timepoint Date
-- NB: this example is indifferent of time
convert :: Date -> Something -> Double
convert _ (Currency USD) = 1
convert _ (Currency EUR) = 1.2
convert _ (Currency CHF) = 1.1
convert _ (Currency GBP) = 1.4
convert _ Good = 500
convert _ (Material Pipe) = 2500
convert _ Service = 25 -- "minimum wage"
convert _ (Ton (Crop Wheat)) = 200 -- TO CHANGE
convert _ (Ton (Crop Oat)) = 200 -- TO CHANGE
convert _ (Ton (Crop Barley)) = 200 -- TO CHANGE
convert _ (Ton (Crop Soybeans)) = 200 -- TO CHANGE
convert _ (Ton (Crop Corn)) = 200 -- TO CHANGE
convert _ (Right (Grow _ Wheat)) = 0
convert _ (Right (Grow _ Oat)) = 0
convert _ (Right (Grow _ Barley)) = 0
convert _ (Right (Grow _ Soybeans)) = 0
convert _ (Right (Grow _ Corn)) = 0
convert _ (Labour Worker) = 20
convert _ (Labour JuniorLawyer) = 50
convert _ (Labour SeniorLawyer) = 100
convert _ (Labour JuniorConsultant) = 50
convert _ (Labour SeniorConsultant) = 100
convert _ _ = 0

convertIO :: Something -> IO Double
convertIO k = lift2 convert date (return k)

-- Seasonal Yield Per Harvested Acre
ypha :: Crop -> Double
ypha ck
    | ck == Wheat = 2.3
    | ck == Soybeans = 1.27
    | ck == Oat = 0.99
    | ck == Barley = 1.24
    | ck == Corn = 4.14
    | otherwise = error $ show ck ++ " is not a crop."
----------------------------------------------------------------

----------------------------------------------------------------
--- UTILITY FUNCTIONS FOR CONTRACTS

-- Extends contract to date t and scales all underlings' transaction amounts of x
extend :: Double -> Date -> Contract -> Contract
extend x t c = scale x $ changeDate t c

extendIO :: Double -> Date -> ContractIO -> ContractIO
extendIO x t c = scaleIO (konst x) $ changeDateIO (return t) c

-- Renews contract, the execution date of the new one being at t
renew :: Date -> Contract -> Contract
renew t c = c `and` changeDate t c

renewIO :: Date -> ContractIO -> ContractIO
renewIO t c = c `andIO` changeDateIO (return t) c

-- Makes n copies of contract
copy :: Integer -> Contract -> Contract
copy 0 c = []
copy n c = c `and` copy (n-1) c

copyIO :: Integer -> ContractIO -> ContractIO
copyIO 0 c = zeroIO
copyIO n c = c `andIO` copyIO (n-1) c
----------------------------------------------------------------