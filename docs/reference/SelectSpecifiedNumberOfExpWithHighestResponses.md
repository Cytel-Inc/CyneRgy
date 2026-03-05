# Select Treatments with the Highest Number of Responses

This function is used for the Multi-Arm Multi-Stage (MAMS) design with a
binary outcome and performs treatment selection at the interim analysis
(IA). At the IA, the user-specified number of experimental treatments
(`QtyOfArmsToSelect`) that have the largest number of responses are
selected. After the IA, randomization is based on user-specified inputs:
1:`Rank1AllocationRatio`:`Rank2AllocationRatio` (control, selected
experimental arm with the highest number of responses, selected
experimental arm with the second highest number of responses).

## Usage

``` r
SelectSpecifiedNumberOfExpWithHighestResponses(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
```

## Arguments

- SimData:

  A dataframe consisting of data generated in the current simulation.

- DesignParam:

  A list of design and simulation parameters required to perform
  treatment selection.

- LookInfo:

  A list containing design and simulation parameters that might be
  required to perform treatment selection.

- UserParam:

  A list of user-defined parameters in East or East Horizon. The default
  is `NULL`. The list must contain the following named elements:

  QtyOfArmsToSelect

  :   A value defining how many treatment arms are chosen to advance.
      Note this number must match the number of user-specified
      allocation values.

  Rank1AllocationRatio

  :   A value specifying the allocation to the arm with the highest
      response.

  Rank2AllocationRatio

  :   A value specifying the allocation to the arm with the next highest
      response.

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
# Example 1: Assuming the allocation in the second part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 1 and 2 both have an allocation ratio of 2.
vAllocationRatio    <- c(2, 2)
nErrorCode          <- 0
lReturn             <- list(
  TreatmentID = vSelectedTreatments, 
  AllocRatio  = vAllocationRatio,
  ErrorCode   = nErrorCode
)
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

# Example 2: Assuming the allocation in the second part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many participants as Experimental 1 or Control. 
vAllocationRatio    <- c(1, 2)
nErrorCode          <- 0
lReturn             <- list(
  TreatmentID = vSelectedTreatments, 
  AllocRatio  = vAllocationRatio,
  ErrorCode   = nErrorCode
)
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
