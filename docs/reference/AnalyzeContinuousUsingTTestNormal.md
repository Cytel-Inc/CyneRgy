# Analyze Continuous Data Using t-Test

Performs hypothesis testing using the
[`t.test()`](https://rdrr.io/r/stats/t.test.html) function in base R to
analyze continuous data under the assumption of a normal distribution.
This function demonstrates how analysis and decision-making can be
modified in a simple approach. The test statistic is compared to the
upper boundary computed and sent by East as an input. Note that this
example does not include a futility rule.

## Usage

``` r
AnalyzeContinuousUsingTTestNormal(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame that contains simulated data for the current simulation.

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

  A list of user-defined parameters in East or East Horizon. The default
  is `NULL`. For this example, user-defined parameters are not included.

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
