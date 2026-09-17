# Simulate Two-Arm Continuous Outcomes With Random Structural-Zero Probabilities

Calls the beta-distribution variant from the common
`2ArmNormalOutcomePatientSimulation` example.

## Usage

``` r
SimulatePatientOutcomePercentAtZeroBetaDist(
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

A list in the format required by the response integration point.
