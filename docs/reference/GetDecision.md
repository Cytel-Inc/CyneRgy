# Determine Decision Based on Decision String, Design and Look Information

This function takes a string indicating the desired decision
("Efficacy", "Futility", or "Continue"), design parameters, and look
information, and returns the appropriate decision value. If `LookInfo`
is not NULL, the function uses `LookInfo$RejType` to help determine the
design type:

- **LookInfo\$RejType Codes**:

  - *Efficacy Only*:

    - 1-Sided Efficacy Upper = 0

    - 1-Sided Efficacy Lower = 2

  - *Futility Only*:

    - 1-Sided Futility Upper = 1

    - 1-Sided Futility Lower = 3

  - *Efficacy and Futility*:

    - 1-Sided Efficacy Upper & Futility Lower = 4

    - 1-Sided Efficacy Lower & Futility Upper = 5

  - *Additional Scenarios Not in East Horizon Explore*:

    - 2-Sided Efficacy Only = 6

    - 2-Sided Futility Only = 7

    - 2-Sided Efficacy & Futility = 8

    - Equivalence = 9

The function also uses `DesignParam$TailType` to determine tail
direction:

- 0: Left-tailed

- 1: Right-tailed

Based on the design type and tail direction, the function evaluates the
decision and returns the corresponding integer decision value. Errors
are raised for invalid input combinations.

## Usage

``` r
GetDecision(strDecision, DesignParam, LookInfo)
```

## Arguments

- strDecision:

  A string indicating the desired decision: "Efficacy", "Futility", or
  "Continue".

- DesignParam:

  A list containing design parameters sent from East Horizon Explore to
  the R integration for analysis.

- LookInfo:

  A list containing look information sent from East Horizon Explore to
  the R integration for analysis.
