# Analyze Binary Data Using a Beta-Binomial Model

Computes the posterior probability that the experimental response rate
is greater than the control response rate. At an interim look, efficacy
and futility are determined by `dUpperCutoffEfficacy` and
`dLowerCutoffForFutility`. At the final look, efficacy is declared when
the posterior probability exceeds the efficacy cutoff.

## Usage

``` r
AnalyzeUsingBetaBinomial(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Data frame containing `Response` and `TreatmentID`, where treatment
  `0` is control and treatment `1` is experimental.

- DesignParam:

  List containing `TailType`. For compatibility with fixed-design
  inputs, it may also contain `MaxCompleters`.

- LookInfo:

  Optional list describing the current look. When supplied, it must
  contain `CurrLookIndex`, `NumLooks`, `CumCompleters`, and `RejType`.

- UserParam:

  List containing `dAlphaCtrl`, `dBetaCtrl`, `dAlphaExp`, `dBetaExp`,
  `dUpperCutoffEfficacy`, and `dLowerCutoffForFutility`.

## Value

A list containing posterior probability `TestStat`, integer `ErrorCode`,
integer `Decision`, and posterior mean difference `Delta`. Missing
`UserParam` values produce fatal `ErrorCode = -1`.
