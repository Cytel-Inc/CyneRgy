# Generate Correlated Multiple-Endpoint Responses

Calls the response implementation from the common `MEPPatientSimulation`
example.

## Usage

``` r
GenerateMEPResponse(
  NumPat,
  NumArms,
  TreatmentID,
  ArrivalTime,
  EndpointType,
  EndpointName,
  RespParams,
  Correlation,
  UserParam = NULL
)
```

## Arguments

- NumPat:

  Integer number of patients.

- NumArms:

  Integer number of trial arms.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- ArrivalTime:

  Numeric subject arrival times.

- EndpointType:

  Endpoint type identifiers.

- EndpointName:

  Endpoint names.

- RespParams:

  Response parameters arranged by endpoint and arm.

- Correlation:

  Numeric endpoint correlation inputs.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the MEP response integration point.
