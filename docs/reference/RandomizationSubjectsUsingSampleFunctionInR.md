# Randomize Subjects to Two Arms Using R's `sample()` Function

Calls the [`sample()`](https://rdrr.io/r/base/sample.html)
implementation from the common `RandomizeSubjects` example.

## Usage

``` r
RandomizationSubjectsUsingSampleFunctionInR(
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
