# Analyze Binary Data Using the prop.test Function

This function analyzes binary data using the `prop.test` function in
base R. The calculated p-value from `prop.test` is used to compute the Z
statistic, which is then compared to the upper boundary provided as an
input by East. Note that this example does not include a futility rule.

## Usage

``` r
AnalyzeBinaryUsingPropTest(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame containing the data generated in the current simulation.

- DesignParam:

  List of design and simulation parameters required to perform the
  analysis.

- LookInfo:

  A list containing input parameters related to multiple looks, which
  the user may need to compute test statistics and perform tests. Users
  should access the variables using their names (e.g.,
  `LookInfo$NumLooks`) rather than by their order. Important variables
  in group sequential designs include:

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

  A list of user-defined parameters in East or East Horizon. Default is
  `NULL`. No user parameters are defined for this example.

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
