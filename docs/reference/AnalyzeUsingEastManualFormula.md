# Analyze Binary Outcomes Using the Manual Test-Statistic Formula

Calls the manual-formula implementation from the common
`2ArmBinaryOutcomeAnalysis` example.

## Usage

``` r
AnalyzeUsingEastManualFormula(
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
