# Select Experimental Treatments Using P-value Comparison

At the interim analysis, experimental treatments are compared to the
control using a chi-squared test. Treatments with p-values less than
`dMaxPValue` are selected for stage 2. If no treatments meet the
threshold, the treatment with the smallest p-value is selected. In the
second stage, the randomization ratio will be 1:1
(experimental:control).

## Usage

``` r
SelectExpWithPValueLessThanSpecified(
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

  List of design and simulation parameters required for treatment
  selection.

- LookInfo:

  List containing design and simulation parameters that might be
  required for treatment selection.

- UserParam:

  A list of user-defined parameters in East or East Horizon. Default is
  NULL. The list must contain the following named element:

  UserParam\$dMaxPValue

  :   A value (0,1) specifying the chi-squared probability threshold for
      selecting treatments to advance. Treatments with p-values less
      than this threshold will advance to the second stage.

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

## Examples

``` r
# Example 1: Allocation in the second stage is 1:2:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 1 and Experimental 2 both have an allocation ratio of 2.
vAllocationRatio    <- c(2, 2)
nErrorCode          <- 0
lReturn             <- list(TreatmentID = vSelectedTreatments, 
                             AllocRatio  = vAllocationRatio,
                             ErrorCode   = nErrorCode)
return(lReturn)
#> $TreatmentID
#> [1] 1 2
#> 
#> $AllocRatio
#> [1] 2 2
#> 
#> $ErrorCode
#> [1] 0
#> 

# Example 2: Allocation in the second stage is 1:1:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many patients as Experimental 1 or Control.
vAllocationRatio    <- c(1, 2)
nErrorCode          <- 0
lReturn             <- list(TreatmentID = vSelectedTreatments, 
                             AllocRatio  = vAllocationRatio,
                             ErrorCode   = nErrorCode)
return(lReturn)
#> $TreatmentID
#> [1] 1 2
#> 
#> $AllocRatio
#> [1] 1 2
#> 
#> $ErrorCode
#> [1] 0
#> 
```
