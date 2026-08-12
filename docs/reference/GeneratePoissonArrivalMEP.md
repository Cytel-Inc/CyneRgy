# Generate Patient Arrival Times for an MEP Design

Calls the multiple-endpoint implementation from the common
`GeneratePoissonArrival` example.

## Usage

``` r
GeneratePoissonArrivalMEP(NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL)
```

## Arguments

- NumPat:

  Integer number of patients.

- NumPrd:

  Integer number of accrual periods.

- PrdStart:

  Numeric period start times.

- AccrRate:

  Numeric accrual rates by period.

- UserParam:

  Optional list of user-defined parameters.

## Value

A list in the format required by the arrival integration point.
