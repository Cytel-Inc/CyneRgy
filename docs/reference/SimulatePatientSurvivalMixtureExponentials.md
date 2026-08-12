# Simulate Survival Outcomes From a Mixture of Exponentials

Calls the mixture-of-exponentials implementation from the common
time-to-event patient simulation example.

## Usage

``` r
SimulatePatientSurvivalMixtureExponentials(
  NumSub,
  NumArm,
  ArrivalTime,
  TreatmentID,
  SurvMethod,
  NumPrd,
  PrdTime,
  SurvParam,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects.

- NumArm:

  Integer number of trial arms.

- ArrivalTime:

  Numeric subject arrival times.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- SurvMethod:

  Integer identifying how survival parameters are supplied.

- NumPrd:

  Integer number of survival periods.

- PrdTime:

  Numeric period start times.

- SurvParam:

  Numeric survival parameters arranged as required by `SurvMethod`.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the response integration point.
