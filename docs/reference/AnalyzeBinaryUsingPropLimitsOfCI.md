# Analyze Binary Data Using Proportion Limits of Confidence Interval

This function analyzes binary data using a simplified confidence
interval (CI) limits design. It determines whether to make a "Go" or "No
Go" decision based on the treatment difference and user-specified
thresholds for the CI lower and upper limits. The analysis uses the
`prop.test` function from base R to compute CIs at a user-defined
confidence level.

The decision logic is as follows:

- If the lower limit (LL) of the CI is greater than
  `UserParam$dLowerLimit`, a "Go" decision is made.

- If a "Go" decision is not made, and the upper limit (UL) of the CI is
  less than `UserParam$dUpperLimit`, a "No Go" decision is made.

- Otherwise, continue to the next analysis.

- At the final analysis:

  - If LL \> `UserParam$dLowerLimit`, a "Go" decision is made.

  - Otherwise, a "No Go" decision is made.

## Usage

``` r
AnalyzeBinaryUsingPropLimitsOfCI(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  A data frame containing the data generated in the current simulation.

- DesignParam:

  A list of design and simulation parameters required for the analysis.

- LookInfo:

  A list containing input parameters related to multiple looks, which
  are used to compute test statistics and perform tests. Important
  variables include:

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

  A list of user-defined parameters with the following required
  elements:

  dLowerLimit

  :   A value (0,1) specifying the lower limit, e.g., Minimum Acceptable
      Value (MAV).

  dUpperLimit

  :   A value (0,1) specifying the upper limit for the confidence
      interval, e.g., Target Value (TV).

  dConfLevel

  :   A value (0,1) specifying the confidence level for the `prop.test`
      function.

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

In this example, the boundary information computed and sent from East or
East Horizon is ignored to implement this decision approach.
