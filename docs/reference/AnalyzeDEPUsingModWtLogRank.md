# Analyze Survival Dual Endpoints With a Modestly Weighted Log-Rank Test

Calls the modestly weighted log-rank implementation from the common
`DEPAnalysis` example. This function requires the suggested `survival`
package.

## Usage

``` r
AnalyzeDEPUsingModWtLogRank(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame containing the simulated dual-endpoint patient data.

- DesignParam:

  List of design and simulation parameters.

- LookInfo:

  Optional list describing the current analysis look.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the DEP analysis integration point.
