# Analyze Time-to-Event Data Using Hazard Ratio Limits of Confidence Interval

This function analyzes time-to-event data using a simplified design
based on upper and lower confidence boundaries for hazard ratios (HR).
It determines whether to make a "Go" or "No Go" decision by assessing
the likelihood of the HR being below specified limits. Specifically, the
function utilizes the `coxph()` model from the survival package to
estimate the log hazard ratio and its standard error. The
decision-making process is as follows:

- If the upper limit (UL) of the confidence interval is below the
  Minimum Acceptable Value (MAV), a "Go" decision is made.

- If the lower limit (LL) of the confidence interval is above the Target
  Value (TV), a "No Go" decision is made.

- Otherwise, the analysis continues to the next look.

At the final analysis:

- If UL \< MAV, a "Go" decision is made.

- Otherwise, a "No Go" decision is made.

HR and log HR are monotonically related. Since `coxph()` outputs results
for log HR, the function uses the log HR scale for decision-making.

## Usage

``` r
AnalyzeTTEUsingHazardRatioLimitsOfCI(
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

  A list containing input parameters related to multiple looks. Users
  should access variables using their names, such as:

  - `LookInfo$NumLooks`: Integer representing the number of looks in the
    study.

  - `LookInfo$CurrLookIndex`: Integer representing the current index
    look, starting from 1.

  - `LookInfo$CumEvents`: Vector of cumulative number of events at each
    look.

  - `LookInfo$RejType`: Code representing rejection types, with possible
    values:

    - **Efficacy Only**:

      - `0`: 1-Sided Efficacy Upper.

      - `2`: 1-Sided Efficacy Lower.

    - **Futility Only**:

      - `1`: 1-Sided Futility Upper.

      - `3`: 1-Sided Futility Lower.

    - **Efficacy and Futility**:

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

- HazardRatio:

  A double representing the computed hazard ratio.

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
