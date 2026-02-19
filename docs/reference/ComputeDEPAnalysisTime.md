# Compute Analysis Time for Dual Endpoint (DEP) Designs

Computes the calendar analysis time for trials with one or two
endpoints, supporting both Fixed Sample Designs and Group Sequential
Designs. The function determines when the planned number of events or
completers has been reached based on simulated trial data and design
parameters.

For Group Sequential Designs, the analysis time is determined according
to the current interim look and synchronization endpoint. For Fixed
Sample Designs, the analysis time corresponds to the final look when all
required events or completers are observed.

## Usage

``` r
ComputeDEPAnalysisTime(SimData, DesignParam, LookInfo = NULL)
```

## Arguments

- SimData:

  A data frame containing the simulated subject-level data with the
  following required columns:

  ClndrRespTime

  :   Response times for Endpoint 1.

  CensorIndOrg

  :   Censoring indicators for Endpoint 1.

  ClndrRespTime2

  :   Response times for Endpoint 2.

  CensorIndOrg2

  :   Censoring indicators for Endpoint 2.

- DesignParam:

  A list of design parameters containing:

  EndpointType

  :   Numeric vector specifying endpoint types (1 = completer, 2 =
      event).

  EndpointName

  :   Character vector of endpoint names.

  PlanEndTrial

  :   Integer specifying which endpoint(s) define the trial end:

      1

      :   Both endpoints.

      2

      :   Endpoint 1 only.

      3

      :   Endpoint 2 only.

  MaxCompleters

  :   List of target numbers of completers for each endpoint.

  MaxEvents

  :   List of target numbers of events for each endpoint.

  SampleSize

  :   Total sample size.

- LookInfo:

  Optional list specifying Group Sequential Design information (default
  = `NULL`). When provided, indicates that a GSD is used and must
  include:

  SyncInterim

  :   Endpoint ID used for interim look positioning.

  NumLooks

  :   Total number of looks planned.

  CurrLookIndex

  :   Current look index.

  NumEndpointLooks

  :   Numeric vector giving the number of looks per endpoint.

  CumEvents

  :   List of cumulative event targets per endpoint.

  CumCompleters

  :   List of cumulative completer targets per endpoint.

## Value

A numeric value representing the calendar analysis time (in the same
units as `ClndrRespTime`).
