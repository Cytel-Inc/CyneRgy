# Integration Point: Response - Continuous Outcome with Repeated Measures

[$`\leftarrow`$ Go back to the *Integration Point: Response*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointResponse.md)

Important: **ArrivalTime** is a new required parameter. Existing R
scripts must be updated to include this parameter in the function
definition, even if it is not used.

## Input Variables

When creating a custom R script, you can optionally use specific
variables provided by East Horizon’s engine itself. These variables are
automatically available and do not need to be set by the user, except
for the `UserParam` variable. Refer to the table below for the variables
that are available for this integration point and outcome.

[TABLE]

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description** |
|----|----|
| List | A named list containing `Response1`, `Response2`, …, `ResponseNumVisit`, and `ErrorCode`. |

### Expected Members of the Output List

[TABLE]

Note: Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

## Minimal Template

Your R script could contain a function such as this one, with a name of
your choice. All input variables must be declared, even if they are not
used in the script. Optional parameters such as `ArrivalTime` may be
omitted. We recommend always declaring `UserParam` as a default `NULL`
value in the function arguments, as this will ensure that the same
function will work regardless of whether the user has specified any
custom parameters in East Horizon.

A detailed template with step-by-step explanations is available here:
[SimulatePatientOutcome.RepeatedMeasures.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.RepeatedMeasures.R)

    GenerateResponse <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vOutResponse      <- c()
      lReturn           <- list()
      
      # Add code to simulate the patient data as desired.  
      # Example of how to create the return list with Response1, Response2, ..., ResponseNumVisit.
      # Store the generated continuous response values in a vector called vOutResponse for each subject.
      # Store the response vector in a list called lReturn for each visit.
      for(i in 1:NumVisit)
      {
          strVisitName              <- paste0( "Response", i ) # Response1, Response2, ..., ResponseNumVisit
          vOutResponse              <- rep( 0, NumSub )  # Initializing response array to 0 
          lReturn[[ strVisitName ]] <- as.double( vOutResponse )
      }
        
      lReturn$ErrorCode <- as.integer( nError )
      return( lReturn )
    }

## Example

Explore the following example for more context:

1.  [**2-Arm, Normal Outcome, Repeated Measures - Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmNormalRepeatedMeasuresResponseGeneration.md)
    - [GenerateResponseDiffOfMeansRepeatedMeasures.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmNormalRepeatedMeasuresResponseGeneration/R/GenerateResponseDiffOfMeansRepeatedMeasures.R)
