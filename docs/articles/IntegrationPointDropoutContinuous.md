# Integration Point: Dropout - Continuous Outcome

[$`\leftarrow`$ Go back to the *Integration Point: Dropout*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointDropout.md)

## Input Variables

When creating a custom R script, you can optionally use specific
variables provided by East Horizon’s engine itself. These variables are
automatically available and do not need to be set by the user, except
for the `UserParam` variable. Refer to the table below for the variables
that are available for this integration point, outcome, and study
objective.

[TABLE]

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description**                                      |
|----------|------------------------------------------------------|
| List     | A named list containing `CensorInd` and `ErrorCode`. |

### Expected Members of the Output List

[TABLE]

Note: Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

## Minimal Template

Your R script could contain a function such as this one, with a name of
your choice. All applicable input variables must be declared, even if
they are not used in the script. Input variables that are not applicable
(depending on the study objective) must not be declared. We recommend
always declaring `UserParam` as a default `NULL` value in the function
arguments, as this will ensure that the same function will work
regardless of whether the user has specified any custom parameters in
East Horizon.

Detailed templates with step-by-step explanations are available here:
[Dropout.BinaryAndContinuous.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Dropout.BinaryAndContinuous.R)
for 2-Arm and
[Dropout.BinaryAndContinuous.MAMS.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Dropout.BinaryAndContinuous.MAMS.R)
for Multiple Arm.

### For `Study Objective = Two Arm Confirmatory`

    GenCensorInd <- function( NumSub, ProbDrop, UserParam = NULL )
    {
      nError                <- 0 # Error handling (no error)
      vCensoringIndicator   <- rep( 1, NumSub ) # Initializing vector to 1 (all patients are completers)    
      
      # Write the actual code here.
      # Store the generated censor indicator values in a vector called vCensoringIndicator

      return( list( CensorInd = as.integer( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
    }

### For `Study Objective = Multiple Arm Confirmatory`

    GenCensorInd <- function( NumSub, NumArm, ProbDrop, TreatmentID, UserParam = NULL )
    {
      nError                <- 0 # Error handling (no error)
      vCensoringIndicator   <- rep( 1, NumSub ) # Initializing vector to 1 (all patients are completers)    
      
      # Write the actual code here.
      # Store the generated censor indicator values in a vector called vCensoringIndicator

      return( list( CensorInd = as.integer( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**2-Arm, Single Endpoint - Simulate Patient
    Dropout**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmPatientDropout.md)
    - [GenerateCensoringUsingBinomialProportion.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmPatientDropout/R/GenerateCensoringUsingBinomialProportion.R)
2.  [**Multiple Arm, Time-to-Event Outcome - Simulate Patient
    Dropout**](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmPatientDropout.md)
    - [GenerateCensoringMultiArmUsingBinomialProportion.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/MultiArmPatientDropout/R/GenerateCensoringMultiArmUsingBinomialProportion.R)
