# Select Experimental Treatments Using a Bayesian Rule

This function implements the MAMS design for binary outcomes and
performs treatment selection at the interim analysis (IA) using a
Bayesian decision rule. At IA, an experimental treatment is selected for
stage 2 if its posterior probability of exceeding a user-specified
historical response rate (`UserParam$dHistoricResponseRate`) is greater
than a user-defined threshold (`UserParam$dMinPosteriorProbability`):
`Pr(pj > UserParam$dHistoricResponseRate | data) > UserParam$dMinPosteriorProbability`.
If no treatment satisfies this criterion, the treatment with the highest
posterior probability is selected. All experimental arms assume the same
prior distribution:
`pj ~ Beta(UserParam$dPriorAlpha, UserParam$dPriorBeta)`. For stage 2,
selected treatments are randomized against the control arm in a 2:1
ratio (experimental:control).

## Usage

``` r
SelectExpUsingBayesianRule(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  Dataframe containing data generated in the current simulation.

- DesignParam:

  List of design and simulation parameters required to perform treatment
  selection.

- LookInfo:

  List containing design and simulation parameters, which might be
  required to perform treatment selection.

- UserParam:

  A list of user-defined parameters in East or East Horizon. The default
  is NULL. The list must contain the following named elements:

  UserParam\$dPriorAlpha

  :   A value (0,1) defining the prior alpha parameter of the beta
      distribution.

  UserParam\$dPriorBeta

  :   A value (0,1) specifying the prior beta parameter of the beta
      distribution.

  UserParam\$dHistoricResponseRate

  :   A value (0,1) specifying the historic response rate.

  UserParam\$dMinPosteriorProbability

  :   A value (0,1) specifying the posterior probability needed to
      exceed the historic response rate for experimental treatment
      selection.

## Value

A list containing:

- TreatmentID:

  A vector of experimental treatment IDs selected to advance, e.g., 1,
  2, ..., number of experimental treatments.

- AllocRatio:

  A vector of allocation ratios for the selected treatments relative to
  control.

- ErrorCode:

  An integer indicating success or error status:

  ErrorCode = 0

  :   No error.

  ErrorCode \> 0

  :   Nonfatal error, current simulation aborted but subsequent
      simulations will run.

  ErrorCode \< 0

  :   Fatal error, no further simulations attempted.

## Note

- The length of `TreatmentID` and `AllocRatio` must be the same.

- The allocation ratio for control is always 1, and `AllocRatio` values
  are relative to this. For example, an allocation value of 2 means
  twice as many participants are randomized to the experimental
  treatment compared to control.

- The order of `AllocRatio` should match `TreatmentID`, with
  corresponding elements assigned their respective allocation ratios.

- The returned vector includes only `TreatmentID` values for
  experimental treatments. For example, `TreatmentID = c(0, 1, 2)` is
  invalid because control (`0`) should not be included.

- At least one treatment and one allocation ratio must be returned.
