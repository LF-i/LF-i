module Simulate where

import Base
import Prelude hiding (when, and, or, until, Right)

--------------------------------------------------------------------
--- SIMULATE

-- Sums up all amounts of contracts with currency k 
aggregate :: Something -> Contract -> Double -- possibility of more contracts, although impossible
aggregate k []     = 0
aggregate k (c:cs) = if something c == k then amount c + aggregate k cs else aggregate k cs

-- Executes all contracts in a portfolio
simulate :: Portfolio -> Portfolio
simulate d = aggregateAll d . sortByDate $ [ a | a <- contract d, owner d == holder a ] ++ give [ a | a <- contract d, owner d == counterparty a ]
    where
        aggregateAll :: Portfolio -> Contract -> Portfolio
        aggregateAll d [] = d
        aggregateAll d (c:cs) 
            |   eur d     + aggregate (Currency EUR) c < 0  ||
                chf d     + aggregate (Currency CHF) c < 0  ||
                (usd d    + aggregate (Currency USD) c) < 0 ||
                gbp d     + aggregate (Currency GBP) c < 0  ||
                good d    + aggregate Good c < 0            ||
                service d + aggregate Service c < 0 = error $ owner d ++ "'s deposit cannot fulfill contract " ++ show c
            | otherwise = aggregateAll Portfolio {
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

-- Add contract to portfolio and "apply" it to the latter (activate porfolio).
addAndActivateIO :: IO PortfolioIO -> ContractIO -> IO PortfolioIO
addAndActivateIO d cs = do
    d <- d
    cs <- cs
    return $ fromToIO' d cs
    where 
        fromToIO' :: PortfolioIO -> [TransactionIO] -> PortfolioIO
        fromToIO' d [] = d
        fromToIO' d (c:cs)
            |   eurIO d + aggregate (Currency EUR) c < 0      || 
                chfIO d + aggregate (Currency CHF) c < 0    ||
                usdIO d + aggregate (Currency USD) c < 0    ||
                gbpIO d + aggregate (Currency GBP) c < 0    ||
                goodIO d + aggregate Good c < 0 ||
                serviceIO d + aggregate Service c < 0 = error $ "The deposit cannot fulfill contract " ++ show c
            |   otherwise = fromToIO' 
                            PortfolioIO {
                                eurIO = eurIO d + aggregate (Currency EUR) c,
                                chfIO = chfIO d + aggregate (Currency CHF) c,
                                usdIO = usdIO d + aggregate (Currency USD) c,
                                gbpIO = gbpIO d + aggregate (Currency GBP) c,
                                goodIO = goodIO d + aggregate Good c,
                                serviceIO = serviceIO d + aggregate Service c,
                                contractIO = contractIO d 
                                } cs
        aggregate :: Something -> TransactionIO -> Double
        aggregate k c = if something2 c == k then amount2 c else 0

-- Activate porfolio: the contracts will be executed/applied to the Portfolio
activateIO :: IO PortfolioIO -> IO PortfolioIO
activateIO d = do
    d <- d
    let cs = contractIO d
    return $ fromToIO' d cs
    where 
        fromToIO' :: PortfolioIO -> [TransactionIO] -> PortfolioIO
        fromToIO' d [] = d
        fromToIO' d (c:cs)
            |   eurIO d + aggregate (Currency EUR) c < 0   || 
                chfIO d + aggregate (Currency CHF) c < 0   ||
                usdIO d + aggregate (Currency USD) c < 0   ||
                gbpIO d + aggregate (Currency GBP) c < 0   ||
                goodIO d + aggregate Good c < 0            ||
                serviceIO d + aggregate Service c < 0 = error $ "The deposit cannot fulfill contract " ++ show c
            | otherwise = fromToIO' 
                            PortfolioIO {
                            eurIO = eurIO d + aggregate (Currency EUR) c,
                            chfIO = chfIO d + aggregate (Currency CHF) c,
                            usdIO = usdIO d + aggregate (Currency USD) c,
                            gbpIO = gbpIO d + aggregate (Currency GBP) c,
                            goodIO = goodIO d + aggregate Good c,
                            serviceIO = serviceIO d + aggregate Service c,
                            contractIO = contractIO d
                            } cs
        aggregate :: Something -> TransactionIO -> Double
        aggregate k c = if something2 c == k then amount2 c else 0

-- Remove cntract from portfolio
removeContractIO :: IO PortfolioIO -> Int -> ContractIO -> IO PortfolioIO
removeContractIO d n c = do
    d <- d
    c <- c
    let tot = length [ a | a <- contractIO d,  a == head c ]
    if tot - n >= 0 then return PortfolioIO {  eurIO = eurIO d,
                                            chfIO = chfIO d,
                                            usdIO = usdIO d,
                                            gbpIO = gbpIO d,
                                            goodIO = goodIO d,
                                            serviceIO = serviceIO d,
                                            contractIO = [ a | a <- contractIO d,  a == head c ] ++ take (tot-n) [ a | a <- contractIO d,  a == head c ] }
    else error $ "Contract " ++ show c ++ " is not in deposit " ++ show d ++ "."


--------------------------------------------------------------------