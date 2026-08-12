# Analyze Repeated-Measures Outcomes

Calls the GLS repeated-measures implementation from the common
`2ArmNormalRepeatedMeasuresAnalysis` example. This function requires the
suggested `nlme` package. Multi-look analyses also require the suggested
`rpact` package.

## Usage

``` r
Analyze.RepeatedMeasures(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame containing the simulated repeated-measures data.

- DesignParam:

  List of design and simulation parameters.

- LookInfo:

  Optional list describing the current analysis look.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the analysis integration point.
