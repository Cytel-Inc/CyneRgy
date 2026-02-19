# Integration Point: Response - Binary Outcome

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

| **Variable** | **Type** | **Description** |
|----|----|----|
| **NumSub** | Integer | Number of subjects in the trial. |
| **NumArm** | Integer | Number of arms in the trial ﴾including placebo/control, and experimental﴿. |
| **ArrivalTime** | Vector of Numeric | Vector of length `NumSub`, indicating the arrival time for each subject. |
| **TreatmentID** | Vector of Integer | Vector of length `NumSub`, indicating the allocation of subjects to arms. Index `0` represents placebo/control. For example, `[0, 0, 1]` indicates three subjects: two in the control group and one experimental. |
| **PropResp** | Vector of Numeric | Vector of length `NumArm`, indicating the expected proportions (probabilities) of responders for each arm. The first element corresponds to the control group, followed by the probabilities for each experimental arm in sequence. |
| **FollowUpDur** | Numeric | Follow-up duration, acts as By Time for response proportions. Only applicable for Vaccine Efficacy (`Better Response = Smaller Value`, `Test = 1 - Ratio of Proportions`). |
| **OneMinusROP** | Numeric | Value of 1 - Ratio of Proportions. Only applicable for Vaccine Efficacy (`Better Response = Smaller Value`, `Test = 1 - Ratio of Proportions`). |
| **UserParam** | List | Contains all user-defined parameters specified in the East Horizon interface (refer to the [Instructions](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointResponse.html#instructions) section). To access these parameters in your R code, use the syntax: `UserParam$NameOfTheVariable`, replacing `NameOfTheVariable` with the appropriate parameter name. |

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
(depending on standard binary outcome vs. Vaccine Efficacy) must not be
declared. We recommend always declaring `UserParam` as a default `NULL`
value in the function arguments, as this will ensure that the same
function will work regardless of whether the user has specified any
custom parameters in East Horizon. For Vaccine Efficacy, `OneMinusROP`
must be declared with default value `NULL`.

A detailed template with step-by-step explanations is available here:
[SimulatePatientOutcome.Binary.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/SimulatePatientOutcome.Binary.R)

### For standard binary outcome (`Better Response = Larger Value`, `Test = Difference of Proportions or Custom`)

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vPatientOutcome   <- rep( 0, NumSub ) # Initializing response array to 0  
      
      # Write the actual code here.
      # Store the generated binary response values in a vector called vPatientOutcome.

      return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
    }

### For Vaccine Efficacy (`Better Response = Smaller Value`, `Test = 1 - Ratio of Proportions`)

    GenerateResponse <- function( NumSub, NumArm, ArrivalTime, TreatmentID, FollowUpDur, PropResp, OneMinusROP = NULL, UserParam = NULL )
    {
      nError            <- 0 # Error handling (no error)
      vPatientOutcome   <- rep( 0, NumSub ) # Initializing response array to 0  
      
      # Write the actual code here.
      # Store the generated binary response values in a vector called vPatientOutcome.

      return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**2-Arm, Binary Outcome - Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmBinaryOutcomePatientSimulation.md)
    - [SimulatePatientOutcomePercentAtZero.Binary.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomePatientSimulation/R/SimulatePatientOutcomePercentAtZero.Binary.R)
    - [SimulatePatientOutcomePercentAtZeroBetaDist.Binary.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomePatientSimulation/R/SimulatePatientOutcomePercentAtZeroBetaDist.Binary.R)
2.  [**Multiple Arm, Patient
    Simulation**](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmPatientSimulation.md)
    - [SimulatePatientOutcomeMultiArmPercentAtZero.Binary.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/MultiArmPatientSimulation/R/SimulatePatientOutcomeMultiArmPercentAtZero.Binary.R)
