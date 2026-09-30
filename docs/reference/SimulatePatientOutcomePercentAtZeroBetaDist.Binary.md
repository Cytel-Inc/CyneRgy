# Simulate Binary Outcomes With Random Structural-Zero Probabilities

Calls the beta-distribution variant from the common
`2ArmBinaryOutcomePatientSimulation` example.

## Usage

``` r
SimulatePatientOutcomePercentAtZeroBetaDist.Binary(
  NumSub,
  NumArm,
  ArrivalTime,
  TreatmentID,
  PropResp,
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

- PropResp:

  Numeric response probability for each arm.

- UserParam:

  Optional list of user-defined parameters described in the complete
  example.

## Value

A list in the format required by the response integration point.
