{-# LANGUAGE FlexibleContexts #-}
module CommunalPaymentsCalc where


import Safe (readMay)
import Control.Monad.Except


-- import CommunalPaymentsCalc.Internal

newtype OthersCount = OthersCount Int deriving (Show, Eq)

newtype WaterBill = WaterBill Float deriving (Show, Eq)

newtype InternetBill = InternetBill Float deriving (Show, Eq)

newtype ElectricityRate = ElectricityRate Float deriving (Show, Eq)

newtype ElectricityPrevMonthAmount = ElectricityPrevMonthAmount Int deriving (Show, Eq)

newtype ElectricityCurMonthAmount = ElectricityCurMonthAmount Int deriving (Show, Eq)


data CommunalPayments =
  CommunalPayments { cpOthersCount :: OthersCount
                   , cpWaterBill :: WaterBill
                   , cpInternetBill :: InternetBill
                   , cpElectricityRate :: ElectricityRate
                   , cpElectricityPrevMonthAmount :: ElectricityPrevMonthAmount
                   , cpElectricityCurMonthAmount :: ElectricityCurMonthAmount
                   } deriving (Show, Eq)


-- emptyCommunalPayments :: CommunalPayments
-- emptyCommunalPayments = CommunalPayments { cpWaterBill = WaterBill 0
--                                          , cpInternetBill = InternetBill 0
--                                          , cpElectricityRate = ElectricityRate 0
--                                          , cpElectricityPrevMonthAmount = ElectricityPrevMonthAmount 0
--                                          , cpElectricityCurMonthAmount = ElectricityCurMonthAmount 0 }


communalPayments :: OthersCount
                    -> WaterBill
                    -> InternetBill
                    -> ElectricityRate
                    -> ElectricityPrevMonthAmount
                    -> ElectricityCurMonthAmount
                    -> CommunalPayments
communalPayments = CommunalPayments


readVal :: (Read a, MonadError String m) => (a -> b) -> String -> String -> m b
readVal f errMessage s = maybe onError (return . f) (readMay s)
  where onError = throwError $ errMessage ++ ": " ++ s

-- readWaterBill :: MonadError String m => String -> m WaterBill
-- readWaterBill s = case readMay s of
--                     Just x -> return . WaterBill $ x
--                     Nothing -> throwError $ "incorrect water bill value: " ++ s

readOthersCount :: MonadError String m => String -> m OthersCount
readOthersCount = readVal OthersCount "incorrect people count"

readWaterBill :: MonadError String m => String -> m WaterBill
readWaterBill = readVal WaterBill "incorrect water bill value"

readInternetBill :: MonadError String m => String -> m InternetBill
readInternetBill = readVal InternetBill "incorrect internet bill value"

readElectricityRate :: MonadError String m => String -> m ElectricityRate
readElectricityRate = readVal ElectricityRate "incorrect electricity rate"

readElectricityPrevMonthAmount :: MonadError String m => String -> m ElectricityPrevMonthAmount
readElectricityPrevMonthAmount = readVal ElectricityPrevMonthAmount "wrong eelctricity previous month amount"

readElectricityCurMonthAmount :: MonadError String m => String -> m ElectricityCurMonthAmount
readElectricityCurMonthAmount = readVal ElectricityCurMonthAmount "wrong electricity current month amount"


-- totalPlayments :: CommunalPayments ->
