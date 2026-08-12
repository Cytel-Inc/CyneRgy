# Simulate Continuous Patient Outcomes with Probability of Zero from a Beta Distribution

Simulates patient outcomes from a normal distribution, with the
probability of a zero outcome being random and sampled from a Beta
distribution. The probability of a zero outcome is determined as
follows:

- For the control treatment, it is sampled from a
  \\Beta(UserParam\$dCtrlBetaParam1, UserParam\$dCtrlBetaParam2)\\
  distribution.

- For the experimental treatment, it is sampled from a
  \\Beta(UserParam\$dExpBetaParam1, UserParam\$dExpBetaParam2)\\
  distribution. This approach incorporates variability in the unknown
  probability of no response.

## Usage

``` r
SimulateContinuousPatientOutcomePercentAtZeroBetaDist(
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

  UserParam\$dCtrlBetaParam1

  :   Numeric; first parameter in the Beta distribution for the control
      (Treatment 1).

  UserParam\$dCtrlBetaParam2

  :   Numeric; second parameter in the Beta distribution for the control
      (Treatment 1).

  UserParam\$dExpBetaParam1

  :   Numeric; first parameter in the Beta distribution for the
      experimental (Treatment 2).

  UserParam\$dExpBetaParam2

  :   Numeric; second parameter in the Beta distribution for the
      experimental (Treatment 2).

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
