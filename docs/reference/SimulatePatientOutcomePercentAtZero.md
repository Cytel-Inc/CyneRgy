# Simulate Two-Arm Continuous Outcomes

Calls the implementation from the common
`2ArmNormalOutcomePatientSimulation` example. Outcomes follow the
arm-specific normal distributions, with optional arm-specific
probabilities of a structural zero in `UserParam`.

## Usage

``` r
SimulatePatientOutcomePercentAtZero(
  NumSub,
  ArrivalTime,
  TreatmentID,
  Mean,
  StdDev,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects.

- ArrivalTime:

  Numeric subject arrival times.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- Mean:

  Numeric arm-specific means.

- StdDev:

  Numeric arm-specific standard deviations.

- UserParam:

  Optional list of user-defined parameters described in the complete
  example.

## Value

A list in the format required by the corresponding integration point.
