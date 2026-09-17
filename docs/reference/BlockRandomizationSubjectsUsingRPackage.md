# Permuted Block Randomization for Two-Armed Trials

Calls the permuted-block implementation from the common
`RandomizeSubjects` example. This function requires the suggested
`randomizeR` package.

## Usage

``` r
BlockRandomizationSubjectsUsingRPackage(
  NumSub,
  NumArms,
  AllocRatio,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects to randomize.

- NumArms:

  Integer number of trial arms. This function supports two arms.

- AllocRatio:

  Numeric experimental-to-control allocation ratio.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the randomization integration point.
