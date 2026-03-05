# Integration Point: Response - Multiple Endpoints

[$`\leftarrow`$ Go back to the *Integration Point: Response*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointResponse.md)

## Input Variables

When creating a custom R script, you can optionally use specific
variables provided by East Horizon’s engine itself. These variables are
automatically available and do not need to be set by the user, except
for the `UserParam` variable. Refer to the table below for the variables
that are available for this integration point and endpoint.

[TABLE]

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description** |
|----|----|
| List | A named list containing one or multiple of: `Response`, `ArrivalRank`, `Corr`, and `ErrorCode`. |

### Expected Members of the Output List

[TABLE]

**Note:** Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

**Note:** “Endpoint 1” is used as a sample endpoint name. It will be the
actual endpoint name as specified by the `EndpointName` input.

## Minimal Template

Your R script could contain a function such as this one, with a name of
your choice. All applicable input variables must be declared, even if
they are not used in the script. We recommend always declaring
`UserParam` as a default `NULL` value in the function arguments, as this
will ensure that the same function will work regardless of whether the
user has specified any custom parameters in East Horizon.

A detailed template with step-by-step explanations is available here:
[SimulatePatientOutcome.MEP.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.MEP.R).

    GenerateResponse <- function( NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      
      lResponse          <- list()
      
      # Initialize responses
      vPatientOutcomeEP1  <- rep( 0, NumPat )  
      vPatientOutcomeEP2  <- rep( 0, NumPat )
      vPatientOutcomeEP3  <- rep( 0, NumPat )  
      vPatientOutcomeEP4  <- rep( 0, NumPat )
      vPatientOutcomeEP5  <- rep( 0, NumPat )
      
      # Write the actual code here.
      
      lResponse[[EndpointName[[1]]]] <- vPatientOutcomeEP1
      lResponse[[EndpointName[[2]]]] <- vPatientOutcomeEP2
      lResponse[[EndpointName[[3]]]] <- vPatientOutcomeEP3
      lResponse[[EndpointName[[4]]]] <- vPatientOutcomeEP4
      lResponse[[EndpointName[[5]]]] <- vPatientOutcomeEP5

      return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nError ) ) )
    }

## Example

Explore the following example for more context:

1.  [**Multiple Endpoints - Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/MEPPatientSimulation.md)
    - [GenerateMEPResponse.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/MEPPatientSimulation/R/GenerateMEPResponse.R)
