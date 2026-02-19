# Integration Point: Dropout - Continuous Outcome with Repeated Measures

[$`\leftarrow`$ Go back to the *Integration Point: Dropout*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointDropout.md)

This integration point and outcome is currently available for the **Two
Arm Confirmatory** Study Objective.

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
| List | A named list containing `ErrorCode` and one of the following: `DropOutTime`, `DropoutVisitID`, or `CensorInd1`, `CensorInd2`, …, `CensorIndNumVisit`. |

The output list can take one of the three forms below.

### Option 1: Expected Members of the Output List

[TABLE]

Note: Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

### Option 2: Expected Members of the Output List

[TABLE]

Note: Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

### Option 3: Expected Members of the Output List

[TABLE]

Note: Additional custom variables can be included as members of the
output list. All outputs will automatically be available as input
variables for analysis or treatment selection endpoints in the `SimData`
variable as described here: [Variables of
SimData](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md).

## Minimal Templates

Your R script could contain a function such as these ones, with a name
of your choice. All input variables must be declared, even if they are
not used in the script. We recommend always declaring `UserParam` as a
default `NULL` value in the function arguments, as this will ensure that
the same function will work regardless of whether the user has specified
any custom parameters in East Horizon.

A detailed template with step-by-step explanations is available here:
[Dropout.RepeatedMeasures.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Dropout.RepeatedMeasures.R)

### Minimal Template for Option 1

    GenDropTimes <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                              DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vDropoutTime      <- rep( Inf, NumSub ) # Initializing dropout times vector to Inf (all patients are completers)  
      
      # Write the actual code here.
      # Store the generated dropout times in a vector called vDropoutTime.
      
      return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError ) ) )
    }

### Minimal Template for Option 2

    GenDropVisitID <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                                DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vDropoutVisitID   <- rep( 1, NumSub ) # Initializing dropout visit IDs vector to 1 (all patients drop out after first visit)  
      
      # Write the actual code here.
      # Store the generated dropout visit IDs in a vector called vDropoutVisitID.
      
      return( list( DropoutVisitID = as.double( vDropoutVisitID ), ErrorCode = as.integer( nError ) ) )
    }

### Minimal Template for Option 3

    GenCensorInd <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                              DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vCensorInd        <- c()
      lReturn           <- list()
      
      # Add code to simulate the dropout data as desired. 
      # Example of how to create the return list with CensorInd1, CensorInd2, ..., CensorIndNumVisit.
      # Store the generated censor indicator values in a vector called vCensorInd for each subject.
      # Store the response vector in a list called lReturn for each visit.
      for(i in 1:NumVisit)
      {
        strCensorIndName              <- paste0( "CensorInd", i ) # CensorInd1, CensorInd2, ..., CensorIndNumVisit
        vCensorInd                    <- rep( 1, NumSub ) # Initializing censor array to 1 (all patients are completers)    
        lReturn[[ strCensorIndName ]] <- as.integer( vCensorInd )
      }
      
      lReturn$ErrorCode <- as.integer( nError )
      return( lReturn )
    }

## Examples

Explore the following examples for more context:

1.  [**2-Arm, Single Endpoint - Simulate Patient
    Dropout**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmPatientDropout.md)
    - [GenerateDropoutTimeForRM.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmPatientDropout/R/GenerateDropoutTimeForRM.R)
