# Analyze Binary Data Using Formula 28.2 from the East Manual

This function computes the test statistic using formula 28.2 from the
East manual, which is designed for analyzing binary data in group
sequential designs. It demonstrates how analysis and decision-making can
be customized in a simple approach. The test statistic is compared to
the upper efficacy boundary provided by East or East Horizon as input.
Note that this example does not include a futility rule.

## Usage

``` r
AnalyzeBinaryUsingEastManualFormula(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  A data frame containing data generated in the current simulation.

- DesignParam:

  A list of design and simulation parameters required for analysis.

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

  A list of user-defined parameters provided by East or East Horizon.
  Defaults to `NULL`. In this example, user-defined parameters are not
  included.

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
