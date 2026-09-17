# Generate Dropout Times for Survival Outcomes

Calls the survival implementation from the common `2ArmPatientDropout`
example.

## Usage

``` r
GenerateDropoutTimeForSurvival(
  NumSub,
  NumArm,
  TreatmentID,
  DropMethod,
  NumPrd,
  PrdTime,
  DropParam,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects.

- NumArm:

  Integer number of trial arms.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- DropMethod:

  Integer identifying the dropout model.

- NumPrd:

  Integer number of dropout periods.

- PrdTime:

  Numeric period start times.

- DropParam:

  Numeric dropout parameters.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the dropout integration point.
