# Simulate Binary Patient Outcomes

Simulates a binary response for each subject. `UserParam` can define
arm-specific probabilities that a subject is treatment resistant and
therefore always has response `0`. Without `UserParam`, outcomes are
sampled directly from `PropResp`.

## Usage

``` r
SimulatePatientOutcomePercentAtZero.Binary(
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

  Numeric subject arrival times. Retained for compatibility with the
  response integration point.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- PropResp:

  Numeric response probability for each arm.

- UserParam:

  Optional list containing `dProbOfTreatmentResistantCtrl` and
  `dProbOfTreatmentResistantExp`.

## Value

A list containing numeric `Response` and integer `ErrorCode`.
