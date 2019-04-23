module Main where

import Control.Arrow
import Data.List
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- TODO: We really want to understand whether it is better to repay up front or
-- invest what you would pay and use interest for repayments. We need to explore
-- different scenarios of investments.

-- TODO: Use a moving pay scale (some RLE list of salary)
-- [50000, 500000, 50000, 50000, 55000, 550000] == [(3, 50000), (2, 55000)]

-- TODO: Hook up to webpage with updateable parameters (loan amount, salary
-- profile, investment projections, RPI projections)


main :: IO ()
main = toFile fileOptions "salary_repayments.svg" $ do
  layout_title .= "Salaries against Total Loan Repayments over 30 years"

  layout_x_axis . laxis_title .= "Yearly Salary"
  layout_y_axis . laxis_title .= "Total Loan Repayments"

  -- Set the color for the line plot to blue
  setColors [opaque blue]

  plot (line "" [signal initialPrincipal [25000, 26000 .. 150000]])
  where fileOptions = def { _fo_format = SVG }


signal :: Double -> [Double] -> [(Double, Double)]
signal initialPrincipal salaries =
  (id &&& (totalRepayment initialPrincipal . repayment)) <$> salaries


initialPrincipal :: Double
initialPrincipal = 56324.0


repaymentThreshold :: Double
repaymentThreshold = 25000.0


-- | The amount repaid per year for a given salary
--
--   This is calculated as 9% of all income over the repayment threshold
repayment :: Double -> Double
repayment salary = (salary - repaymentThreshold) * 0.09


-- | Calculate the schedule of repayments
schedule :: Double -> Double -> [Double]
schedule initialPrincipal maxRepayment = unfoldr nextSchedule initialPrincipal
 where
  nextSchedule principal = case nextRepayment principal maxRepayment of
    0         -> Nothing
    repayment -> Just (repayment, nextPrincipal principal repayment)


-- | Calculate the total repayments made over 30 years
totalRepayment :: Double -> Double -> Double
totalRepayment initialPrincipal maxRepayment =
  sum . take 30 $ schedule initialPrincipal maxRepayment


-- | Given a principal and an annual repayment, calculate the next principal
nextPrincipal :: Double -> Double -> Double
nextPrincipal principal repayment = (principal - repayment) * assumedInterest
  where assumedInterest = 1.063


-- | Given a principal and a maximum repayment, calculate the next repayment
nextRepayment :: Double -> Double -> Double
nextRepayment principal repayment
  | principal > repayment = repayment
  | otherwise = principal
