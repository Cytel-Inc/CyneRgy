# Simulate TTE Patient Outcomes from a Weibull Distribution

This function simulates patient data from a Weibull (shape, scale)
distribution. The `rweibull` function in the `stats` package is used to
simulate the survival time. See help on `rweibull`.

The required function signature for integration with East or East
Horizon includes the `SurvMethod`, `NumPrd`, `PrdTime`, and `SurvParam`,
which are ignored in this function, and only the parameters in
`UserParam` are utilized.

## Usage

``` r
SimulateTTEPatientWeibull(
  NumSub,
  NumArm,
  TreatmentID,
  SurvMethod,
  NumPrd,
  PrdTime,
  SurvParam,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  The number of patient times to generate for the trial. This is a
  single numeric value, e.g., 250.

- NumArm:

  The number of arms in the trial, a single numeric value. For a two-arm
  trial, this will be 2.

- TreatmentID:

  A vector of treatment IDs. `0 = treatment 1`, `1 = treatment 2`. The
  length of `TreatmentID` must be equal to `NumSub`.

- SurvMethod:

  This value is pulled from the Input Method drop-down list.

  1

  :   Hazard Rate.

  2

  :   Cumulative percentage survival.

  3

  :   Medians.

- NumPrd:

  Number of time periods that are provided.

- PrdTime:

  If `SurvMethod = 1`

  :   `PrdTime` is a vector of starting times of hazard pieces.

  If `SurvMethod = 2`

  :   Times at which the cumulative percentage survivals are specified.

  If `SurvMethod = 3`

  :   `PrdTime` is 0 by default.

- SurvParam:

  A 2-D array of parameters to generate the survival times, depending on
  the table in the Response Generation tab.

  If `SurvMethod = 1`

  :   `SurvParam` is an array (`NumPrd` rows, `NumArm` columns) that
      specifies arm-by-arm hazard rates (one rate per arm per piece).
      Thus, `SurvParam[i, j]` specifies the hazard rate in the `i`th
      period for the `j`th arm. Arms are in columns where column 1 is
      control and column 2 is experimental. Time periods are in rows,
      where row 1 is time period 1, row 2 is time period 2, etc.

  If `SurvMethod = 2`

  :   `SurvParam` is an array (`NumPrd` rows, `NumArm` columns) that
      specifies arm-by-arm the cumulative percentage survivals (one
      value per arm per piece). Thus, `SurvParam[i, j]` specifies the
      cumulative percentage survivals in the `i`th period for the `j`th
      arm.

  If `SurvMethod = 3`

  :   `SurvParam` will be a `1 x 2` array with median survival times for
      each arm. Column 1 is control, column 2 is experimental.

- UserParam:

  A list of user-defined parameters. Must contain the following named
  elements:

  `UserParam$dShapeCtrl`

  :   The shape parameter in the Weibull distribution for the control
      treatment.

  `UserParam$dScaleCtrl`

  :   The scale parameter in the Weibull distribution for the control
      treatment.

  `UserParam$dShapeExp`

  :   The shape parameter in the Weibull distribution for the
      experimental treatment.

  `UserParam$dScaleExp`

  :   The scale parameter in the Weibull distribution for the
      experimental treatment.

## Value

A list with the following components:

- `SurvivalTime`:

  A vector of simulated survival times for patients.

- `ErrorCode`:

  Optional integer value:

  0

  :   No error.

  \> 0

  :   Non-fatal error; current simulation is aborted but subsequent
      simulations continue.

  \< 0

  :   Fatal error; no further simulations are attempted.
