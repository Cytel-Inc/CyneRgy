# Integration Point: Response - Dual Endpoints (TTE-TTE or TTE-Binary)

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
that are available for this integration point and endpoint.

[TABLE]

**Note:** “Endpoint 1” is used as a sample endpoint name. It will be the
actual endpoint name as specified by the user.

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description**                                     |
|----------|-----------------------------------------------------|
| List     | A named list containing `Response` and `ErrorCode`. |

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
(depending on TTE-TTE vs. TTE-Binary) must not be declared. We recommend
always declaring `UserParam` as a default `NULL` value in the function
arguments, as this will ensure that the same function will work
regardless of whether the user has specified any custom parameters in
East Horizon.

A detailed template with step-by-step explanations is available here:
[SimulatePatientOutcome.DEP.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.DEP.R).

### For `Dual Endpoint = TTE-TTE`

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, EndpointType, EndpointName, SurvMethod, NumPrd, PrdTime, SurvParam, Correlation, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      
      Response          <- list()
      Response[[EndpointName[[1]]]] <- rep( 0, NumSub ) # Initializing survival times array of the first endpoint to 0
      Response[[EndpointName[[2]]]] <- rep( 0, NumSub ) # Initializing survival times array of the second endpoint to 0
      
      # Write the actual code here.
      # Store the generated survival times values in each array of the list Response.

      return( list( Response = as.list( vResponse ), ErrorCode = as.integer( nError ) ) )
    }

### For `Dual Endpoint = TTE-Binary`

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, EndpointType, EndpointName, SurvMethod, NumPrd, PrdTime, SurvParam, PropResp, Correlation, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      
      Response          <- list()
      Response[[EndpointName[[1]]]] <- rep( 0, NumSub ) # Initializing response array of the first endpoint to 0
      Response[[EndpointName[[2]]]] <- rep( 0, NumSub ) # Initializing response array of the second endpoint to 0
      
      # Write the actual code here.
      # Store the generated response values in each array of the list Response.

      return( list( Response = as.list( vResponse ), ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**Dual Endpoints - Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/DEPPatientSimulation.md)
    - [SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/DEPPatientSimulation/R/SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece.R)
    - [SimulatePatientOutcomeDEPSurvBinSingleHazardPiece.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/DEPPatientSimulation/R/SimulatePatientOutcomeDEPSurvBinSingleHazardPiece.R)
