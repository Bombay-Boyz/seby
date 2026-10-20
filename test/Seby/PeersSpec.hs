{-# LANGUAGE OverloadedStrings #-}

module Seby.PeersSpec (runTests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Seby.Analysis.Peers
import Seby.Domain.Money (Money, fromRupees)
import Seby.Domain.Period (FiscalPeriod (..), mkFiscalYear)
import Seby.Domain.Types (CompanyId (..), Rate, StatementBasis (..), mkRate, unRate)

runTests :: IO ()
runTests = do
  fy <- expectRight "fiscal year" (mkFiscalYear 2026)
  target <- profile (CompanyId "TGT") "Target" (GeneralIndustry "Software") [SubscriptionBased, BusinessToBusiness] (AnnualPeriod fy) Standalone (fromRupees 1000) (fromRupees 2000)
  same <- profile (CompanyId "AAA") "Same" (GeneralIndustry "Software") [SubscriptionBased, BusinessToBusiness] (AnnualPeriod fy) Standalone (fromRupees 1000) (fromRupees 2000)
  smaller <- profile (CompanyId "BBB") "Smaller" (GeneralIndustry "Software") [BusinessToBusiness] (AnnualPeriod fy) Standalone (fromRupees 500) (fromRupees 1000)
  bank <- profile (CompanyId "BNK") "Bank" Bank [Regulated] (AnnualPeriod fy) Standalone (fromRupees 1000) (fromRupees 2000)
  let selection = rankPeers target [smaller, same, target, bank]
  case selectedPeers selection of
    firstPeer : _ -> assert "exact match scores 100" (candidateScore firstPeer == 100)
    [] -> fail "eligible peer selection unexpectedly empty"
  assert "ties break by company id" (map (profileCompanyId . candidateProfile) (selectedPeers selection) == [CompanyId "AAA", CompanyId "BBB"])
  assert "self is excluded" (SameCompany `elem` map snd (excludedPeers selection))
  assert "different industry is excluded" (DifferentIndustry `elem` map snd (excludedPeers selection))
  assert "smaller comparable scores in range" (candidateScore (last (selectedPeers selection)) >= 40 && candidateScore (last (selectedPeers selection)) <= 100)
  assert "bank debt/equity is not applicable" (isNotApplicable (metricApplicability Bank DebtToEquityMetric))
  assert "generic industry keeps return on equity" (metricApplicability (GeneralIndustry "Retail") ReturnOnEquityMetric == Applicable)

  rates <- traverse mkRateOrFail [0, 1, 2, 3, 1000000]
  median <- expectRight "median with high outlier" (medianRate rates)
  assert "median robust to large high outlier" (unRate median == 2)
  assert "four observations rejected" (medianRate (take 4 rates) == Left (TooFewIndustryObservations 5 4))
  evenRates <- traverse mkRateOrFail [1, 2, 3, 4, 1000000, 2000000]
  evenMedian <- expectRight "even sample median" (medianRate evenRates)
  assert "even median averages center pair" (unRate evenMedian == 3.5)
  largeRates <- traverse mkRateOrFail [1 .. 20000]
  largeMedian <- expectRight "large industry sample" (medianRate largeRates)
  assert "20k observations median" (unRate largeMedian == 10000.5)
  putStrLn "Peer analysis tests passed (outliers and 20,000-observation sample included)."

profile :: CompanyId -> Text -> Industry -> [BusinessModel] -> FiscalPeriod -> StatementBasis -> Money -> Money -> IO CompanyProfile
profile identifier name industry models period basis revenue cap =
  expectRight "profile construction" (mkCompanyProfile identifier name industry (toNonEmpty models) period basis revenue cap)
  where
    toNonEmpty [] = SubscriptionBased :| []
    toNonEmpty (first : rest) = first :| rest

mkRateOrFail :: Double -> IO Rate
mkRateOrFail value = expectRight "rate construction" (mkRate value)

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label result = case result of
  Right value -> pure value
  Left err -> fail (label <> ": " <> show err)

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else fail ("failed: " <> label)

isNotApplicable :: IndustryApplicability -> Bool
isNotApplicable (NotApplicableInIndustry _) = True
isNotApplicable Applicable = False
