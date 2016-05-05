{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Console where

-- import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except
import System.IO

import CommunalPaymentsCalc
import Math

type ConsoleT = ExceptT String IO

newtype Console a = Console { cRun :: ConsoleT a }
                    deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadIO
                             , MonadError String)

runConsole :: Console a -> IO (Either String a)
runConsole x = runExceptT $ cRun x

withPrompt :: MonadIO m => String -> m a ->  m a
withPrompt m f =
  do cPrint m
     cPrint ": "
     f

askWithPrompt :: (MonadError String m, MonadIO m) => String -> (String -> m a) -> m a
askWithPrompt m f = (withPrompt m (liftIO getLine) >>= f) `catchError` handleError
  where handleError e =
          do cPrint "error: "
             cPrintLn e
             cPrintLn "try again"
             askWithPrompt m f

cPrint :: MonadIO m => String -> m ()
cPrint m = liftIO $ putStr m

cPrintLn :: MonadIO m => String -> m ()
cPrintLn m = liftIO $ putStrLn m

askOthersCount :: (MonadError String m, MonadIO m) => m (OthersCount)
askOthersCount = askWithPrompt "enter count of other people" readOthersCount


askWaterBill :: (MonadError String m, MonadIO m) => m (WaterBill)
askWaterBill = askWithPrompt "enter water bill" readWaterBill


askInternetBill :: Console (InternetBill)
askInternetBill = askWithPrompt "enter internet bill" readInternetBill


askElectricityRate :: Console (ElectricityRate)
askElectricityRate = askWithPrompt "enter electricity rate" readElectricityRate

askElectricityPrevMonthAmount :: Console (ElectricityPrevMonthAmount)
askElectricityPrevMonthAmount =
  askWithPrompt "enter electricity previous month amount" readElectricityPrevMonthAmount

askElectricityCurMonthAmount :: Console (ElectricityCurMonthAmount)
askElectricityCurMonthAmount =
  askWithPrompt "enter electricity current month amount" readElectricityCurMonthAmount


askCommunalPayments :: Console (CommunalPayments)
askCommunalPayments =
  do cPrintLn "communal payments info: "
     communalPayments <$> askOthersCount <*> askWaterBill
                       <*> askInternetBill
                       <*> askElectricityRate
                       <*> askElectricityPrevMonthAmount
                       <*> askElectricityCurMonthAmount


printPaymentsDetails :: (MonadIO m) => Math -> String -> CommunalPayments -> m ()
printPaymentsDetails math msg cp =
  do cPrintLn msg
     doPrint "electricity" mElectricity
     doPrint "water" mWater
     doPrint "internet" mInternet
     doPrint "total" mTotal
  where doPrint s f=
          do cPrint (s ++ ": ")
             cPrintLn . show . f math $ cp


loop :: IO ()
loop =
  do hSetBuffering stdout NoBuffering
     r <- runConsole $ do
       cp <- askCommunalPayments
       cPrintLn ""
       printPaymentsDetails totalMath "together:" cp
       cPrintLn ""
       printPaymentsDetails otherMath "other:" cp
       cPrintLn ""
       printPaymentsDetails ourMath "we:" cp
     putStrLn ""
     putStrLn (show r)
