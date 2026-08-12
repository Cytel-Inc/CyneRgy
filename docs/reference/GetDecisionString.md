# Generate Decision String Based on Interim and Final Analysis Conditions

This function evaluates look information, efficacy conditions, and
futility conditions to generate the decision string (`strDecision`)
required for the `GetDecision` function. If `LookInfo` is not `NULL`,
the `LookInfo$RejType` parameter can be used to determine the design
type.

`LookInfo$RejType` Codes:

- **Efficacy Only**:

  - 1-Sided Efficacy Upper = 0

  - 1-Sided Efficacy Lower = 2

- **Futility Only**:

  - 1-Sided Futility Upper = 1

  - 1-Sided Futility Lower = 3

- **Efficacy and Futility**:

  - 1-Sided Efficacy Upper and Futility Lower = 4

  - 1-Sided Efficacy Lower and Futility Upper = 5

- **Not in East Horizon Explore Yet**:

  - 2-Sided Efficacy Only = 6

  - 2-Sided Futility Only = 7

  - 2-Sided Efficacy and Futility = 8

  - Equivalence = 9

## Usage

``` r
GetDecisionString(
  LookInfo,
  nLookIndex,
  nQtyOfLooks,
  bIAEfficacyCondition = FALSE,
  bIAFutilityCondition = FALSE,
  bFAEfficacyCondition = FALSE,
  bFAFutilityCondition = FALSE
)
```

## Arguments

- LookInfo:

  List containing look information passed from East Horizon Explore to
  the R integration for analysis.

- nLookIndex:

  Integer indicating the current look index, created by the user in the
  analysis code.

- nQtyOfLooks:

  Integer indicating the total number of looks in the study, created by
  the user in the analysis code.

- bIAEfficacyCondition:

  Logical condition evaluated to determine interim efficacy at a look
  (defaults to `FALSE`).

- bIAFutilityCondition:

  Logical condition evaluated to determine interim futility at a look
  (defaults to `FALSE`).

- bFAEfficacyCondition:

  Logical condition evaluated to determine final efficacy at the last
  look (defaults to `FALSE`).

- bFAFutilityCondition:

  Logical condition evaluated to determine final futility at the last
  look (defaults to `FALSE`).
