# Randomize Subjects Between Two Arms

Randomly assigns subjects to control (`0`) and experimental (`1`) arms
while enforcing the requested final allocation counts.

## Usage

``` r
RandomizationSubjectsUsingUniformDistribution(
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

  Optional list of user-defined parameters. Retained for compatibility
  with the randomization integration point.

## Value

A list containing integer vectors `TreatmentID` and `ErrorCode`.
`ErrorCode` is `0` on success and `-1` when `NumArms` is not two.
