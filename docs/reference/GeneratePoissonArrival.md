# Generate Patient Arrival Times

Generates patient arrival times according to a Poisson process. When
`UserParam` is supplied, its named rates `dRate1`, `dRate2`, and so on
define a one-time-unit accrual ramp-up. Otherwise, `PrdStart` and
`AccrRate` define the accrual periods and rates.

## Usage

``` r
GeneratePoissonArrival(NumSub, NumPrd, PrdStart, AccrRate, UserParam = NULL)
```

## Arguments

- NumSub:

  Integer number of subjects to simulate.

- NumPrd:

  Integer number of accrual periods. Retained for compatibility with the
  arrival integration point.

- PrdStart:

  Numeric vector containing the start time of each accrual period; the
  first value should be `0`.

- AccrRate:

  Numeric vector containing the accrual rate in each period.

- UserParam:

  Optional list of user-defined rates named `dRate1`, `dRate2`, and so
  on.

## Value

A list containing `ArrivalTime`, a numeric vector of length `NumSub`,
and integer `ErrorCode` equal to `0`.
