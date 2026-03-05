# Integration Point: Dropout - Dual Endpoints (TTE-TTE or TTE-Binary)

[$`\leftarrow`$ Go back to the *Integration Point: Dropout*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointDropout.md)

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

| **Type** | **Description**                                        |
|----------|--------------------------------------------------------|
| List     | A named list containing `DropOutTime` and `ErrorCode`. |

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

    GenDropTimes <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vDropoutTime      <- rep( Inf, NumSub ) # Initializing dropout times vector to Inf (all patients are completers)  
      
      # Write the actual code here.
      # Store the generated dropout times in a vector called vDropoutTime

      return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError ) ) )
    }
