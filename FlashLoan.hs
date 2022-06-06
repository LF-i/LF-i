module FlashLoan where

import Base

data Market = Mkt1 | Mkt2 | Mkt3 deriving (Show, Eq, Read)

flashloanIO :: Double -> Currency -> Something -> Market -> Market -> ContractIO
flashloanIO x k1 k2 m1 m2 = do
    date <- date
    zcbIO date (return x) (Currency k1)
        `andIO`
        zcbIO date (return x * convertIO (Currency k1) / convertIO_Mkt k2 m1) k2
        `andIO`
        giveIO (zcbIO date (return x * convertIO (Currency k1) / convertIO_Mkt k2 m2) k2)
        `andIO`
        giveIO (zcbIO date (return x) (Currency k1))

convertIO_Mkt :: Something -> Market -> IO Double
convertIO_Mkt Good m
    | m == Mkt1 = 500
    | m == Mkt2 = 501
    | m == Mkt3 = 502
    | otherwise = 0
convertIO_Mkt _ _ = 0