# Analyze Binary Data Using Beta-Binomial Model

Perform analysis for efficacy using a Beta(\\\alpha\\, \\\beta\\) prior
to compute the posterior probability that the experimental treatment is
better than the control treatment care. The analysis assumes a Bayesian
model and uses posterior probabilities for decision-making:

- **Efficacy:** If \\Pr(\pi\_{Exp} \> \pi\_{Ctrl} \| \text{data}) \>
  \text{Upper Cutoff Efficacy}\\, declare efficacy.

- **Futility:** If \\Pr(\pi\_{Exp} \> \pi\_{Ctrl} \| \text{data}) \<
  \text{Lower Cutoff Futility}\\, declare futility.

- At final analysis (FA): Declare efficacy or futility based on the
  posterior probability.

When simulating under the null case, setting \\dLowerCutoffForFutility =
0\\ provides the false-positive rate for the non-binding futility rule.
Setting \\dLowerCutoffForFutility \> 0\\ provides the operating
characteristics (OC) of the binding futility rule, as the rule is always
followed.

## Usage

``` r
AnalyzeBinaryUsingBetaBinomial(
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

  A list of input parameters necessary to compute the test statistic and
  perform the test. Variables should be accessed using names (e.g.,
  `DesignParam$Alpha`).

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

  dAlphaCtrl

  :   Prior alpha parameter for control treatment (prior successes).

  dBetaCtrl

  :   Prior beta parameter for control treatment (prior failures).

  dAlphaExp

  :   Prior alpha parameter for experimental treatment (prior
      successes).

  dBetaExp

  :   Prior beta parameter for experimental treatment (prior failures).

  dUpperCutoffEfficacy

  :   Upper cutoff (0,1) for efficacy check. Above this value declares
      efficacy.

  dLowerCutoffForFutility

  :   Lower cutoff (0,1) for futility check. Below this value declares
      futility.

  If not specified, a Beta(1, 1) prior is used for both control and
  experimental treatments.

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
