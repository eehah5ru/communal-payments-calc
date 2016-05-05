module Math where

import CommunalPaymentsCalc

data Math = Math { mElectricity :: (CommunalPayments -> Float)
                 , mWater :: CommunalPayments -> Float
                 , mInternet :: CommunalPayments -> Float
                 , mTotal :: CommunalPayments -> Float }


totalMath :: Math
totalMath = Math electricity water internet total
  where electricity :: CommunalPayments -> Float
        electricity cp = let (ElectricityPrevMonthAmount a) = cpElectricityPrevMonthAmount cp
                             (ElectricityCurMonthAmount b) = cpElectricityCurMonthAmount cp
                             (ElectricityRate r) = cpElectricityRate cp
                         in ((fromIntegral b) - (fromIntegral a)) * r

        water :: CommunalPayments -> Float
        water cp = let (WaterBill w) = cpWaterBill cp
                   in w

        internet :: CommunalPayments -> Float
        internet cp = let (InternetBill i) = cpInternetBill cp
                      in i

        total :: CommunalPayments -> Float
        total cp = sum . map (\f -> f cp) $ [electricity, water, internet]


otherMath :: Math
otherMath = Math electricity water internet total
  where electricity :: CommunalPayments -> Float
        electricity cp = ((mElectricity totalMath) cp) / (fromIntegral . flatCommunesCount $ cp)

        water :: CommunalPayments -> Float
        water cp = ((mWater totalMath) cp) / (fromIntegral . flatPeopleCount $ cp)

        internet :: CommunalPayments -> Float
        internet cp = ((mInternet totalMath) cp) / (fromIntegral . flatCommunesCount $ cp)

        total :: CommunalPayments -> Float
        total cp = sum . map (\f -> f cp) $ [electricity, water, internet]


ourMath :: Math
ourMath = Math electricity water internet total
  where electricity :: CommunalPayments -> Float
        electricity = mElectricity otherMath

        water :: CommunalPayments -> Float
        water cp = ((mWater otherMath) cp) * 2.0

        internet :: CommunalPayments -> Float
        internet = mInternet otherMath

        total :: CommunalPayments -> Float
        total cp = sum . map (\f -> f cp) $ [electricity, water, internet]



flatPeopleCount :: CommunalPayments -> Int
flatPeopleCount cp =  let (OthersCount x) = cpOthersCount cp
                      in x + 2

flatCommunesCount :: CommunalPayments -> Int
flatCommunesCount cp = let (OthersCount x) = cpOthersCount cp
                       in x + 1
