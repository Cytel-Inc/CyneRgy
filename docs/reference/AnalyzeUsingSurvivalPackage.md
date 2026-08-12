# Analyze Two-Arm Time-to-Event Outcomes

Calls the log-rank implementation based on the `survival` package from
the common `2ArmTimeToEventOutcomeAnalysis` example.

## Usage

``` r
AnalyzeUsingSurvivalPackage(
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
