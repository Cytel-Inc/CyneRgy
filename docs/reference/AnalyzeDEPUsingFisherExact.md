# Analyze Survival and Binary Dual Endpoints

Calls the Fisher-exact implementation from the common `DEPAnalysis`
example.

## Usage

``` r
AnalyzeDEPUsingFisherExact(
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
