# Convert a Decision Label to an Integration Decision Value

Converts `"Efficacy"`, `"Futility"`, or `"Continue"` into the integer
decision value expected by the analysis integration point. The result
depends on the tail direction, the boundaries enabled by
`LookInfo$RejType`, and whether the current look is interim or final.

Supported `LookInfo$RejType` values are:

- `0` or `2`: efficacy boundary only.

- `1` or `3`: futility boundary only.

- `4` or `5`: efficacy and futility boundaries.

A fixed design is represented by `LookInfo = NULL` and is treated as an
efficacy-only final analysis.

## Usage

``` r
GetDecision(strDecision, DesignParam, LookInfo = NULL)
```

## Arguments

- strDecision:

  Character string equal to `"Efficacy"`, `"Futility"`, or `"Continue"`.

- DesignParam:

  List containing `TailType`, where `0` is left-tailed and `1` is
  right-tailed.

- LookInfo:

  Optional list containing `RejType`, `CurrLookIndex`, and `NumLooks`.

## Value

Integer decision value: `0` for no boundary crossed, `1` for lower
efficacy, `2` for upper efficacy, or `3` for futility.
