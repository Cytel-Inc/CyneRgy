# Generate Dual-Endpoint Decisions

Calls the fixed-sequence decision implementation from the common
`DEPDecisionsUsingMCP` example.

## Usage

``` r
GetDEPDecisionsFSD(
  SimData,
  DesignParam,
  LookInfo = NULL,
  TestStat,
  OutList = NULL,
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

- TestStat:

  Numeric endpoint test statistics.

- OutList:

  Optional output list from the analysis step.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the DEP decision integration point.
