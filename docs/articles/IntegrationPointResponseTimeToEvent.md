# Integration Point: Response - Time-to-Event Outcome

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

| **Type** | **Description**                                         |
|----------|---------------------------------------------------------|
| List     | A named list containing `SurvivalTime` and `ErrorCode`. |

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
used in the script. We recommend always declaring `UserParam` as a
default `NULL` value in the function arguments, as this will ensure that
the same function will work regardless of whether the user has specified
any custom parameters in East Horizon.

Detailed templates with step-by-step explanations are available here:
[SimulatePatientOutcome.TimeToEvent.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.TimeToEvent.R)
and
[SimulatePatientOutcome.TimeToEvent.Stratification.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.TimeToEvent.Stratification.R)
for Stratification.

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vSurvTime         <- rep( 0, NumSub ) # Initializing survival times array to 0    
      
      # Write the actual code here.
      # Store the generated survival time values in a vector called vSurvTime.

      return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = as.integer( nError ) ) )
    }

### For `Stratification` turned on

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, StratumID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vSurvTime         <- rep( 0, NumSub ) # Initializing survival times array to 0    
      
      # Write the actual code here.
      # Store the generated survival time values in a vector called vSurvTime.

      return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**2 Arm, Time-To-Event Outcome - Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmTimeToEventOutcomePatientSimulation.md)
    - [SimulatePatientSurvivalWeibull.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmTimeToEventOutcomePatientSimulation/R/SimulatePatientSurvivalWeibull.R)
    - [SimulatePatientSurvivalMixtureExponentials.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmTimeToEventOutcomePatientSimulation/R/SimulatePatientSurvivalMixtureExponentials.R)
    - [SimulatePatientOutcomeStratification.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmTimeToEventOutcomePatientSimulation/R/SimulatePatientOutcomeStratification.R)
