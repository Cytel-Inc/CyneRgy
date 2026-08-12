# Generate Dropout Times for Repeated-Measures Outcomes

Calls the implementation from the common `2ArmPatientDropout` example.

## Usage

``` r
GenerateDropoutTimeForRM(
  NumSub,
  NumArm,
  NumVisit,
  VisitTime,
  TreatmentID,
  DropMethod,
  ByTime,
  DropParamControl,
  DropParamTrt,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects.

- NumArm:

  Integer number of trial arms.

- NumVisit:

  Integer number of visits.

- VisitTime:

  Numeric vector of visit times.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- DropMethod:

  Integer identifying the dropout model.

- ByTime:

  Logical or integer indicator for time-specific dropout inputs.

- DropParamControl:

  Numeric control-arm dropout parameters.

- DropParamTrt:

  Numeric treatment-arm dropout parameters.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the dropout integration point.
