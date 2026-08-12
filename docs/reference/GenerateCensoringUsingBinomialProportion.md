# Generate Dropout Indicators

Generates an independent censoring indicator for each subject using one
dropout probability. A value of `1` indicates a completer and `0`
indicates a dropout.

## Usage

``` r
GenerateCensoringUsingBinomialProportion(NumSub, ProbDrop, UserParam = NULL)
```

## Arguments

- NumSub:

  Integer number of subjects.

- ProbDrop:

  Numeric dropout probability shared by both arms.

- UserParam:

  Optional list of user-defined parameters. Retained for compatibility
  with the dropout integration point.

## Value

A list containing integer vectors `CensorInd` and `ErrorCode`.
`ErrorCode` is `0` on success.
