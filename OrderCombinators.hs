module OrderCombinators where

import Base


-- ideally orders would appear as pop-ups in the front-end of the trading software
-- this is just a proof-of-concept
-- by wrapping order combinators around the deposit it is possible to combine contracts and deposits

----------------------------------------------------------------
--- ORDER COMBINATORS

market :: IO PortfolioIO -> ContractIO -> IO PortfolioIO
market d c = do
    putStrLn "How many units of the underlying do you want to market (0-∞)?"
    n <- getLine
    let c = copyIO (string2integer n) c
    addContractIO d c

whenP :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a
whenP bool d1 d2 = condP bool d1 d2

anytimeP :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a
anytimeP bool d1 d2 = condP bool (orP d1 d2) d2

condP :: (Show a, Eq a, Read a) => IO Bool -> IO a -> IO a -> IO a -- Bool should be simulated
condP bool c1 c2 = do
    bool <- bool
    if bool then c1 else c2

orP :: (Show a, Eq a, Read a) => IO a -> IO a -> IO a
orP d1 d2 = do
    putStrLn "Would you like to pursue the order (Y/N)?"
    choice <- getLine
    if choice == "Y" then d1 else d2
----------------------------------------------------------------

----------------------------------------------------------------
--- TRADE ORDERS

--- Limit Order
-- The order can be executed at this price or at one more favourable to the investor
limitOrder :: IO PortfolioIO -> IO Bool -> ContractIO -> IO PortfolioIO
limitOrder d bool c = anytimeP bool (market d c) d

--- Stop Order
-- The order is executed at the best available price once the price reaches a particular price or a less-favourable price
stopOrder :: IO PortfolioIO -> IO Bool -> ContractIO -> IO PortfolioIO
stopOrder d bool c = whenP bool (market d c) d

--- Stop-Limit Order
-- The order becomes a limit order as soon as the market price is equal or less favourable than the stop price
stopLimitOrder :: IO PortfolioIO -> IO Bool -> IO Bool -> ContractIO -> IO PortfolioIO
stopLimitOrder d bool1 bool2 c = stopOrder d' bool1 c
    where d' = limitOrder d bool2 c

--- Market-if-Touched (MIT) Order
-- The order is executed at the best available price after a trade occurs at a specified price or at a price more favourable than the specified price.
-- Like a stop order, but ensuring profits are taken rather than limiting a loss.
marketIfTouchedOrder :: IO PortfolioIO -> IO Bool -> ContractIO -> IO PortfolioIO
marketIfTouchedOrder d bool c = stopOrder d bool c

--- Day Order
-- Expires at the end of the trading day (as any order unless otherwise stated)
dayOrder :: IO PortfolioIO -> Date -> ContractIO -> IO PortfolioIO
dayOrder d today c = anytimeP (atIO today date) (market d c) d

--- Time-of-Day Order
-- Specifies a particular period of time during the day when the order can be executed
timeOfDay :: IO PortfolioIO -> (Date,Date) -> ContractIO -> IO PortfolioIO
timeOfDay d (t1,t2) c = anytimeP (betweenIO t1 t2) (market d c) d

--- Fill-or-Kill Order
-- Must be executed immediately on receipt or not at all
fillOrKillOrder :: IO PortfolioIO -> ContractIO -> IO PortfolioIO
fillOrKillOrder d c = orP (market d c) d
----------------------------------------------------------------