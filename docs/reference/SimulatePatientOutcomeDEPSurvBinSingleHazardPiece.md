# Simulate Correlated Survival and Binary Dual Endpoints

Calls the survival/binary implementation from the common
`DEPPatientSimulation` example.

## Usage

``` r
SimulatePatientOutcomeDEPSurvBinSingleHazardPiece(
  NumSub,
  NumArm,
  ArrivalTime = NULL,
  TreatmentID,
  EndpointType,
  EndpointName,
  Correlation,
  SurvMethod,
  NumPrd,
  PrdTime,
  SurvParam,
  PropResp = NULL,
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

- EndpointType:

  Endpoint type identifiers.

- EndpointName:

  Endpoint names.

- Correlation:

  Numeric endpoint correlation inputs.

- SurvMethod:

  Integer identifying how survival parameters are supplied.

- NumPrd:

  Integer number of survival periods.

- PrdTime:

  Numeric period start times.

- SurvParam:

  Numeric survival parameters.

- PropResp:

  Optional numeric binary response probabilities.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the DEP response integration point.
