# Analyze Time-to-Event Outcomes Using a Log-Rank Formula

Calls the manual log-rank implementation from the common time-to-event
analysis example.

## Usage

``` r
AnalyzeUsingEastLogrankFormula(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame containing the simulated patient data.

- DesignParam:

  List of design and simulation parameters.

- LookInfo:

  Optional list describing the current analysis look.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the analysis integration point.
