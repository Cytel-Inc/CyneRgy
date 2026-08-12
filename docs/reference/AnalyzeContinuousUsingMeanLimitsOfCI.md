# Analyze Continuous Data Using Mean Limits of Confidence Interval

This function performs analysis using a simplified limits of confidence
interval design for continuous outcomes. The analysis determines whether
a "Go" or "No-Go" decision is made based on the lower and upper limits
of a user-specified confidence interval. It uses the `t.test` function
from the base R library to compute the confidence interval. The
decision-making process is based on the following logic:

- If the lower limit of the confidence interval (LL) is greater than the
  Minimum Acceptable Value (MAV), a "Go" decision is made.

- If a "Go" decision is not made, and the upper limit of the confidence
  interval (UL) is less than the Target Value (TV), a "No-Go" decision
  is made.

- Otherwise, continue to the next analysis.

- At the final analysis, if LL \> MAV, a "Go" decision is made;
  otherwise, a "No-Go" decision is made.

This function assumes MAV ≤ TV and ignores boundary information sent
from East or East Horizon to implement this decision approach.

## Usage

``` r
AnalyzeContinuousUsingMeanLimitsOfCI(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame consisting of data generated in the current simulation.

- DesignParam:

  List of design and simulation parameters required to perform the
  analysis.

- LookInfo:

  A list of input parameters related to multiple looks in group
  sequential designs. Variables should be accessed by names (e.g.,
  `LookInfo$NumLooks`). Important variables include:

  - `LookInfo$NumLooks`: Integer, number of looks in the study.

  - `LookInfo$CurrLookIndex`: Integer, current look index (starting from
    1).

  - `LookInfo$CumEvents`: Vector, cumulative number of events at each
    look.

  - `LookInfo$RejType`: Code representing rejection types. Possible
    values include:

  - **Efficacy Only:**

    - `0`: 1-Sided Efficacy Upper.

    - `2`: 1-Sided Efficacy Lower.

  - **Futility Only:**

    - `1`: 1-Sided Futility Upper.

    - `3`: 1-Sided Futility Lower.

  - **Efficacy and Futility:**

    - `4`: 1-Sided Efficacy Upper and Futility Lower.

    - `5`: 1-Sided Efficacy Lower and Futility Upper.

- UserParam:

  A list of user-defined parameters. Must contain the following named
  elements:

  UserParam\$dMAV

  :   Numeric; specifies the Minimum Acceptable Value (MAV).

  UserParam\$dTV

  :   Numeric; specifies the Target Value (TV).

  UserParam\$dConfLevel

  :   Numeric (0,1); specifies the confidence level for the
      [`t.test()`](https://rdrr.io/r/stats/t.test.html) function.

## Value

A list containing the following elements:

- TestStat:

  A double representing the computed test statistic.

- Decision:

  Required integer value indicating the decision made:

  0

  :   No boundary crossed (neither efficacy nor futility).

  1

  :   Lower efficacy boundary crossed.

  2

  :   Upper efficacy boundary crossed.

  3

  :   Futility boundary crossed.

  4

  :   Equivalence boundary crossed.

- ErrorCode:

  Optional integer value:

  0

  :   No error.

  \> 0

  :   Non-fatal error; current simulation is aborted but subsequent
      simulations continue.

  \< 0

  :   Fatal error; no further simulations are attempted.

- Delta:

  Estimated difference between experimental and control treatments.

## Note

This function is applicable only when MAV ≤ TV.
