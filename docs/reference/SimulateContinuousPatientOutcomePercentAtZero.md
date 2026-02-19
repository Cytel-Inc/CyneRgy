# Simulate Continuous Patient Outcomes with Proportion at Zero

Simulates patient outcomes from a normal distribution, with a specified
percentage of patients having an outcome of 0. In this example, the
continuous outcome represents a patient's change from baseline. This
function generates patient outcomes such that, on average:

- A specified proportion of patients will have a value of 0 for the
  outcome, as defined by `UserParam`.

- The remaining patients will have their values simulated from a normal
  distribution using the provided mean and standard deviation
  parameters.

## Usage

``` r
SimulateContinuousPatientOutcomePercentAtZero(
  NumSub,
  TreatmentID,
  Mean,
  StdDev,
  UserParam = NULL
)
```

## Arguments

- NumSub:

  The number of subjects to simulate. Must be an integer value.

- TreatmentID:

  A vector of treatment IDs, where:

  - `0` represents Treatment 1.

  - `1` represents Treatment 2. The length of `TreatmentID` must equal
    `NumSub`.

- Mean:

  A numeric vector of length 2 specifying the mean values for the two
  treatments.

- StdDev:

  A numeric vector of length 2 specifying the standard deviations for
  each treatment.

- UserParam:

  A list of user-defined parameters. Must contain the following named
  elements:

  UserParam\$dProbOfZeroOutcomeCtrl

  :   Numeric (0, 1); defines the probability that a patient has an
      outcome of 0 for the control (Treatment 1).

  UserParam\$dProbOfZeroOutcomeExp

  :   Numeric (0, 1); defines the probability that a patient has an
      outcome of 0 for the experimental (Treatment 2).

## Value

A list containing the following elements:

- Response:

  A numeric vector representing the simulated outcomes for each patient.

- ErrorCode:

  Optional integer value:

  0

  :   No error.

  \> 0

  :   Non-fatal error; current simulation is aborted but subsequent
      simulations continue.

  \< 0

  :   Fatal error; no further simulations are attempted.
