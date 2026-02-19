# Integration Point: Analysis - Dual Endpoints (TTE-TTE or TTE-Binary)

[$`\leftarrow`$ Go back to the *Integration Point: Analysis*
page](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointAnalysis.md)

## Input Variables

When creating a custom R script, you can optionally use specific
variables provided by East Horizon’s engine itself. These variables are
automatically available and do not need to be set by the user, except
for the `UserParam` variable. Refer to the table below for the variables
that are available for this integration point, outcome, and study
objective.

| **Variable** | **Type** | **Description** |
|----|----|----|
| **SimData** | Data Frame | Subject data generated in current simulation, one row per subject. To access these variables in your R code, use the syntax: `SimData$NameOfTheVariable`, replacing `NameOfTheVariable` with the appropriate variable name. See below for more information. |
| **DesignParam** | List | Input parameters which may be needed to compute test statistics and perform tests. To access these variables in your R code, use the syntax: `DesignParam$NameOfTheVariable`, replacing `NameOfTheVariable` with the appropriate variable name. See below for more information. |
| **LookInfo** | List | Input parameters related to multiple looks. Not applicable when `Statistical Design = Fixed Sample`. |
| **UserParam** | List | Contains all user-defined parameters specified in the East Horizon interface (refer to the [Instructions](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointAnalysis.html#instructions) section). To access these parameters in your R code, use the syntax: `UserParam$NameOfTheVariable`, replacing `NameOfTheVariable` with the appropriate parameter name. |

### Variables of SimData

[Click here to explore the variables of
SimData.](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfSimData.md)

### Variables of DesignParam

[Click here to explore the variables of
DesignParam.](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfDesignParam.md)

### Variables of LookInfo

[Click here to explore the variables of
LookInfo.](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfLookInfo.md)

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description** |
|----|----|
| List | A named list containing `ErrorCode` and one or multiple of the following members: `TestStat`, `HR`, `Delta`, `AnalysisTime`. See below for more information. |

The output list can take different forms.

### If `LookInfo$FutBdryScale = 2 (Delta scale)`: Expected Members of the Output List

[TABLE]

**Note**: In that case, `Delta` is used to check for futility and
`TestStat` is used to check for efficacy.

### If `LookInfo$FutBdryScale = 6 (HR scale)`: Expected Members of the Output List

[TABLE]

**Note**: In that case, `HR` is used to check for futility and
`TestStat` is used to check for efficacy.

### If `LookInfo$FutBdryScale` is anything else (or no futility boundary): Expected Members of the Output List

[TABLE]

**Note**: In that case, `TestStat` is used to check for both efficacy
and futility.

## Minimal Templates

Your R script could contain a function such as these ones, with a name
of your choice. All input variables must be declared, even if they are
not used in the script. We recommend always declaring `UserParam` as a
default `NULL` value in the function arguments, as this will ensure that
the same function will work regardless of whether the user has specified
any custom parameters in East Horizon.

A detailed template with step-by-step explanations is available here:
[Analyze.DEP.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Analyze.DEP.R).

### Minimal Template for `LookInfo$FutBdryScale = 2 (Delta scale)`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError               <- 0 # Error handling (no error)
        dTestStat            <- 0 # Initialize TestStat
        dDelta               <- 0 # Initialize Delta
        
        # Write the actual code here.
        
        return( list( TestStat = as.double( dTestStat ), Delta = as.double(dDelta), ErrorCode = as.integer( nError ) ) )
    }

### Minimal Template for `LookInfo$FutBdryScale = 6 (HR scale)`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        dTestStat           <- 0 # Initialize TestStat
        dHR                 <- 0 # Initialize HR
        
        # Write the actual code here.
        
        return( list( TestStat = as.double( dTestStat ), HR = as.double(dHR), ErrorCode = as.integer( nError ) ) )
    }

### Minimal Template for other `LookInfo$FutBdryScale` (or no futility boundary)

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        dTestStat           <- 0 # Initialize TestStat
        
        # Write the actual code here.
        
        return( list( TestStat = as.double( dTestStat ), ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**Dual Endpoints -
    Analysis**](https://Cytel-Inc.github.io/CyneRgy/articles/DEPAnalysis.md)
    - [AnalyzeDEPUsingFisherExact.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/DEPAnalysis/R/AnalyzeDEPUsingFisherExact.R)
    - [AnalyzeDEPUsingModWtLogRank.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/DEPAnalysis/R/AnalyzeDEPUsingModWtLogRank.R)
