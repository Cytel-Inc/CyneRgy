# Generate Dropout Indicators for Binary or Continuous Outcomes

Calls the implementation from the common `2ArmPatientDropout` example.
Generates an independent censoring indicator for each subject in binary
or continuous outcome designs using one dropout probability. A value of
`1` indicates a completer and `0` indicates a dropout.

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

  Optional list of user-defined parameters.

## Value

A list in the format required by the dropout integration point.
