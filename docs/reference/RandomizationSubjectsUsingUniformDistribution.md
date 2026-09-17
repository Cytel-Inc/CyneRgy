# Randomize Subjects Between Two Arms

Calls the implementation from the common `RandomizeSubjects` example.
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

  Optional list of user-defined parameters.

## Value

A list in the format required by the randomization integration point.
