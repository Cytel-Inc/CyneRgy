# Generate Multiple-Endpoint Decisions

Calls the decision implementation from the common `MEPDesign` example.

## Usage

``` r
GetMEPDecision(
  SimData,
  AnalysisData,
  DataSummary,
  LookInfo,
  DesignParam,
  OutList,
  UserParam
)
```

## Arguments

- SimData:

  Data frame containing the simulated patient data.

- AnalysisData:

  Analysis data supplied to the design integration point.

- DataSummary:

  Endpoint data summaries.

- LookInfo:

  List describing the current analysis look.

- DesignParam:

  List of design and simulation parameters.

- OutList:

  Output list from the analysis step.

- UserParam:

  List of user-defined parameters.

## Value

A list in the format required by the MEP design integration point.
