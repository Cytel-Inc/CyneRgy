# Generate Repeated-Measures Responses

Calls the response-generation implementation from the common
`2ArmNormalRepeatedMeasuresResponseGeneration` example. This function
requires the suggested `MASS` package.

## Usage

``` r
GenRespDiffOfMeansRepMeasures(
  NumSub,
  NumVisit,
  ArrivalTime,
  TreatmentID,
  Inputmethod,
  VisitTime,
  MeanControl,
  MeanTrt,
  StdDevControl,
  StdDevTrt,
  CorrMat,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  Integer number of subjects.

- NumVisit:

  Integer number of visits.

- ArrivalTime:

  Numeric subject arrival times.

- TreatmentID:

  Integer treatment identifiers beginning at `0`.

- Inputmethod:

  Integer identifying how response parameters are supplied.

- VisitTime:

  Numeric vector of visit times.

- MeanControl:

  Numeric control-arm means by visit.

- MeanTrt:

  Numeric treatment-arm means by visit.

- StdDevControl:

  Numeric control-arm standard deviations by visit.

- StdDevTrt:

  Numeric treatment-arm standard deviations by visit.

- CorrMat:

  Numeric within-subject correlation matrix.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the response integration point.
