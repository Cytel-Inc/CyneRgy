# Randomize Subjects Across Multiple Arms

Calls the multiple-arm implementation from the common
`RandomizeSubjects` example.

## Usage

``` r
RandomizeSubjectsAcrossMultipleArms(
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

  Integer number of trial arms.

- AllocRatio:

  Numeric allocation ratios for the experimental arms relative to the
  control arm; its length is `NumArms - 1`.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the randomization integration point.
