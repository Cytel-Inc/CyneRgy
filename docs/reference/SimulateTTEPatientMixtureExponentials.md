# Simulate TTE Patient Outcomes from a Mixture Exponential Distribution

This function simulates patient data from a mixture of Exponential
distributions. The mixture is based on patient subgroups. For each
subgroup, you specify the median time-to-event for the control and
experimental treatments as well as the probability a patient belongs in
a specific group. The required function signature for integration with
East or East Horizon includes the SurvMethod, NumPrd, PrdTime, and
SurvParam, which are ignored in this function, and only the parameters
in UserParam are utilized.

## Usage

``` r
SimulateTTEPatientMixtureExponentials(
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

  A vector of treatment IDs, 0 = treatment 1, 1 = treatment 2. The
  length of TreatmentID must equal NumSub.

- SurvMethod:

  This value is pulled from the Input Method drop-down list. It will be
  1 (Hazard Rate), 2 (Cumulative percentage survival), or 3 (Medians).

- NumPrd:

  The number of time periods that are provided.

- PrdTime:

  If SurvMethod = 1

  :   PrdTime is a vector of starting times of hazard pieces.

  If SurvMethod = 2

  :   Times at which the cumulative percentage survivals are specified.

  If SurvMethod = 3

  :   Period time is 0 by default.

- SurvParam:

  If SurvMethod = 1

  :   SurvParam is an array (NumPrd rows, NumArm columns) that specifies
      arm-by-arm hazard rates (one rate per arm per piece). Thus,
      SurvParami, j specifies the hazard rate in the ith period for the
      jth arm. Arms are in columns, with column 1 being control and
      column 2 being experimental. Time periods are in rows, with row 1
      being time period 1, row 2 being time period 2, etc.

  If SurvMethod = 2

  :   SurvParam is an array (NumPrd rows, NumArm columns) that specifies
      arm-by-arm cumulative percentage survivals (one value per arm per
      piece). Thus, SurvParami, j specifies the cumulative percentage
      survival in the ith period for the jth arm.

  If SurvMethod = 3

  :   SurvParam will be a 1 x 2 array with median survival times for
      each arm. Column 1 is control, column 2 is experimental.

- UserParam:

  A list of user-defined parameters in East or East Horizon. The default
  is NULL. If UserParam is supplied, it must contain the following:

  UserParam\$QtyOfSubgroups

  :   The quantity of patient subgroups. For each subgroup II =
      1,2,...,QtyOfSubgroups, you must specify ProbSubgroupII,
      MedianTTECtrlSubgroupII, and MedianTTEExpSubgroupII.

  UserParam\$ProbSubgroup1

  :   The probability a patient is in subgroup 1.

  UserParam\$MedianTTECtrlSubgroup1

  :   The median time-to-event for a patient in subgroup 1 that receives
      control treatment.

  UserParam\$MedianTTEExpSubgroup1

  :   The median time-to-event for a patient in subgroup 1 that receives
      experimental treatment.

  UserParam\$ProbSubgroup2

  :   The probability a patient is in subgroup 2.

  UserParam\$MedianTTECtrlSubgroup2

  :   The median time-to-event for a patient in subgroup 2 that receives
      control treatment.

  UserParam\$MedianTTEExpSubgroup2

  :   The median time-to-event for a patient in subgroup 2 that receives
      experimental treatment.

## Value

A list with the following components:

- `SurvivalTime`:

  A vector of simulated survival times for patients.

- `Subgroup`:

  A vector of the patient subgroups.

- `ErrorCode`:

  Optional integer value:

  0

  :   No error.

  \> 0

  :   Non-fatal error; current simulation is aborted but subsequent
      simulations continue.

  \< 0

  :   Fatal error; no further simulations are attempted.
