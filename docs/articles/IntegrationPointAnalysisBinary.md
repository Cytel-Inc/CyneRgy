# Integration Point: Analysis - Binary Outcome

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
| **LookInfo** | List | Input parameters related to multiple looks. Empty when `Statistical Design = Fixed Sample`, but still mandatory in the functions [`CyneRgy::GetDecisionString`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecisionString.md) and [`CyneRgy::GetDecision`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecision.md). See below for more information. |
| **AdaptInfo** | List | Input parameters related to sample size re-estimation. See below for more information. Only applicable when `Statistical Design = Group Sequential with Sample Size Re-Estimation`. |
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

### Variables of AdaptInfo

[Click here to explore the variables of
AdaptInfo.](https://Cytel-Inc.github.io/CyneRgy/articles/VariablesOfAdaptInfo.md)

## Expected Output Variable

East Horizon expects an output of a specific type. Refer to the table
below for the expected output for this integration point:

| **Type** | **Description** |
|----|----|
| List | A named list containing `ErrorCode` and one or multiple of the following members: `Decision`, `AnalysisTime`, `TestStat`, `Delta`, `CtrlCompleters`, `TrmtCompleters`, `CtrlPi`, `StdError`, `ReEstEvents`, `ReEstCompleters`. See below for more information. |

The output list can take different forms.

### For `Study Objective = Two Arm Confirmatory`

#### Option 1 (Decision): Expected Members of the Output List

[TABLE]

**Note for `Statistical Design = Group Sequential`**: When there is no
efficacy boundary to be crossed, the return code of `0` stands for
futility in the final look. Similarly, when there is no futility
boundary to be crossed, the return code of `0` stands for efficacy in
the final look. Use the functions
[`CyneRgy::GetDecisionString`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecisionString.md)
and
[`CyneRgy::GetDecision`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecision.md)
to get decision values in a simple way.

**Note for
`Statistical Design = Group Sequential with Sample Size Re-Estimation`**:
*ReEstCompleters* is required. Only Option 1 is supported, all other
output options are not available.

#### Option 2 (TestStat): Expected Members of the Output List

[TABLE]

**Note for `Statistical Design = Fixed Sample`**: As the design does not
have any futility boundary, `TestStat` will be used to check for
efficacy.

**Notes for `Statistical Design = Group Sequential`**:

- If the design does not have any futility boundary, `TestStat` will be
  used to check for efficacy.
- If `LookInfo$FutBdryScale = 0` (futility boundary scale is Z scale),
  `TestStat` will be used to check for both efficacy and futility.
- If `LookInfo$FutBdryScale = 2` (futility boundary scale is Delta
  scale), `TestStat` will be used to check for efficacy and `Delta` will
  be used to check for futility.
- If `LookInfo$FutBdryScale = 3` (futility boundary scale is conditional
  power scale), `TestStat` will be used to check for efficacy and
  `Delta`, `CtrlCompleters`, `TrmtCompleters`, and `CtrlPi` will be used
  to check for futility.

**Notes for
`Statistical Design = Group Sequential with Sample Size Re-Estimation`**:

See the notes for `Statistical Design = Group Sequential` above for
futility and efficacy checks. Then, if
`Promising Zone Scale = Conditional Power - Estimated`, `Delta` and
`StdError` will be used for sample size re-estimation.

### For `Study Objective = Multiple Arm Confirmatory`

#### Option 1 (Decision): Expected Members of the Output List

[TABLE]

**Notes for `Statistical Design = Group Sequential`**:

- Decision should take care of multiple comparison procedures adjustment
  appropriately.
- When there is no efficacy boundary to be crossed, the return code of
  `0` stands for futility in the final look. Similarly, when there is no
  futility boundary to be crossed, the return code of `0` stands for
  efficacy in the final look. Use the functions
  [`CyneRgy::GetDecisionString`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecisionString.md)
  and
  [`CyneRgy::GetDecision`](https://Cytel-Inc.github.io/CyneRgy/reference/GetDecision.md)
  to get decision values in a simple way.

#### Option 2 (TestStat): Expected Members of the Output List

[TABLE]

**Note for `Statistical Design = Fixed Sample`**: As the design does not
have any futility boundary, `TestStat` will be used to check for
efficacy.

**Notes for `Statistical Design = Group Sequential`**:

- If the design does not have any futility boundary, `TestStat` will be
  used to check for efficacy.
- If `LookInfo$FutBdryScale = 1` (futility boundary scale is adjusted
  p-value scale), `TestStat` will be used to check for both efficacy and
  futility.
- If `LookInfo$FutBdryScale = 2` (futility boundary scale is Delta
  scale), `TestStat` will be used to check for efficacy and `Delta` will
  be used to check for futility.

#### Option 3 (AdjPVal): Expected Members of the Output List

[TABLE]

**Note for `Statistical Design = Fixed Sample`**: As the design does not
have any futility boundary, `AdjPVal` will be used to check for
efficacy.

**Notes for `Statistical Design = Group Sequential`**:

- If the design does not have any futility boundary, `AdjPVal` will be
  used to check for efficacy.
- If `LookInfo$FutBdryScale = 1` (futility boundary scale is adjusted
  p-value scale), `AdjPVal` will be used to check for both efficacy and
  futility.
- If `LookInfo$FutBdryScale = 2` (futility boundary scale is Delta
  scale), `AdjPVal` will be used to check for efficacy and `Delta` will
  be used to check for futility.

#### Option 4 (RawPVal): Expected Members of the Output List

[TABLE]

**Note for `Statistical Design = Fixed Sample`**: As the design does not
have any futility boundary, `RawPVal` will be used to check for
efficacy.

**Notes for `Statistical Design = Group Sequential`**:

- If the design does not have any futility boundary, `RawPVal` will be
  used to check for efficacy.
- If `LookInfo$FutBdryScale = 1` (futility boundary scale is adjusted
  p-value scale), this option cannot be used.
- If `LookInfo$FutBdryScale = 2` (futility boundary scale is Delta
  scale), `RawPVal` will be used to check for efficacy and `Delta` will
  be used to check for futility.

## Minimal Templates

Your R script could contain a function such as these ones, with a name
of your choice. All input variables must be declared, even if they are
not used in the script. We recommend always declaring `UserParam` as a
default `NULL` value in the function arguments, as this will ensure that
the same function will work regardless of whether the user has specified
any custom parameters in East Horizon.

Detailed templates with step-by-step explanations are available here:
[Analyze.Binary.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Analyze.Binary.R)
for 2-Arm,
[Analyze.Binary.SSR.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Analyze.Binary.SSR.R)
for 2-Arm with SSR, and
[Analyze.Binary.MAMS.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Templates/Analyze.Binary.MAMS.R)
for Multiple Arm.

### For `Study Objective = Two Arm Confirmatory`

#### Minimal Template for Option 1 (Decision)

##### For `Statistical Design = Fixed Sample`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        library( CyneRgy )
        nError              <- 0 # Error handling (no error)
        
        # This is an example using GetDecisionString and GetDecision.
        
        # Write the actual code here.
        # It is a fixed sample design, so no interim look nor futility check.
        bFAEfficacyCheck <- TRUE # If TRUE, declares efficacy.
        # Usually, bFAEfficacyCheck would be a conditional statement such as 'dTValue > dBoundary'.
        
        # These variables are set because it is a fixed sample design.
        nQtyOfLooks          <- 1
        nLookIndex           <- 1 
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
        
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                   bFAEfficacyCondition = bFAEfficacyCheck)

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
        
        return( list( Decision = as.integer( nDecision ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        library( CyneRgy )
        nError              <- 0 # Error handling (no error)
        
        # This is an example using GetDecisionString and GetDecision.
        
        # Write the actual code here.
        # It is a group sequential design, so interim looks and futility check are possible.
        bIAEfficacyCheck <- TRUE # If TRUE, declares efficacy at the interim look.
        bIAFutilityCheck <- FALSE # If TRUE, declares futility at the interim look.
        bFAEfficacyCheck <- TRUE # If TRUE, declares efficacy at the final look.
        # Usually, the Check variables would be conditional statements such as 'dTValue > dBoundary'.
        
        # These variables are from LookInfo because it is a group sequential design.
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
        
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                   bIAEfficacyCondition = bIAEfficacyCheck,
                                                   bIAFutilityCondition = bIAFutilityCheck,
                                                   bFAEfficacyCondition = bFAEfficacyCheck )

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
        
        return( list ( Decision = as.integer( nDecision ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential with Sample Size Re-Estimation`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError            <- 0 # Error handling (no error)
        
        nDecision         <- 0 # Initialize decision
        nReEstCompleters  <- 0 # Initialize number of completers
        # Write the actual code here.
        
        return( list ( Decision = as.integer( nDecision ), ReEstCompleters = as.integer( nReEstCompleters ), ErrorCode = as.integer( nError ) ) )
    }

#### Minimal Template for Option 2 (TestStat)

##### For `Statistical Design = Fixed Sample`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        dTestStatistic      <- 0
        
        # Write the actual code here.
        # Store the computed test statistic in dTestStatistic.
        
        return( list( TestStat = as.double( dTestStatistic ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        dTestStatistic      <- 0
        dDelta              <- 0 # Use if futility boundary scale is Delta or conditional power
        dCtrlCompleters     <- 0 # Use if futility boundary scale is conditional power
        dTrmtCompleters     <- 0 # Use if futility boundary scale is conditional power
        dCtrlPi             <- 0 # Use if futility boundary scale is conditional power
        
        
        # Write the actual code here.
        # Store the computed test statistic in dTestStatistic.
        # Compute dDelta, dCtrlCompleters, dTrmtCompleters, dCtrlPi if needed.
        
        return( list( TestStat = as.double( dTestStatistic ),
                      Delta = as.double( dDelta ), # Include if futility boundary scale is Delta or conditional power
                      CtrlCompleters = as.double( dCtrlCompleters ), # Include if futility boundary scale is conditional power
                      TrmtCompleters = as.double( dTrmtCompleters ), # Include if futility boundary scale is conditional power
                      CtrlPi = as.double( dCtrlPi ), # Include if futility boundary scale is conditional power
                      ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential with Sample Size Re-Estimation`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        dTestStatistic      <- 0
        dDelta              <- 0 # Use if futility boundary scale is Delta or conditional power
        dCtrlCompleters     <- 0 # Use if futility boundary scale is conditional power
        dTrmtCompleters     <- 0 # Use if futility boundary scale is conditional power
        dCtrlPi             <- 0 # Use if futility boundary scale is conditional power
        dStdError           <- 0 # Use if Promising Zone Scale = Conditional Power - Estimated
        
        
        # Write the actual code here.
        # Store the computed test statistic in dTestStatistic.
        # Compute dDelta, dCtrlCompleters, dTrmtCompleters, dCtrlPi if needed.
        
        return( list( TestStat = as.double( dTestStatistic ),
                      Delta = as.double( dDelta ), # Include if futility boundary scale is Delta or conditional power
                      CtrlCompleters = as.double( dCtrlCompleters ), # Include if futility boundary scale is conditional power
                      TrmtCompleters = as.double( dTrmtCompleters ), # Include if futility boundary scale is conditional power
                      CtrlPi = as.double( dCtrlPi ), # Include if futility boundary scale is conditional power
                      StdError = as.double(dStdError ), # Include if Promising Zone Scale = Conditional Power - Estimated
                      ErrorCode = as.integer( nError ) ) )
    }

### For `Study Objective = Multiple Arm Confirmatory`

#### Minimal Template for Option 1 (Decision)

##### For `Statistical Design = Fixed Sample`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        library( CyneRgy )
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vDecision           <- rep( 0, NumTreatments )  # Initializing decision vector to 0
        
        # This is an example using GetDecisionString and GetDecision.
        # Write the actual code here.
        
        # These variables are set because it is a fixed sample design.
        nQtyOfLooks          <- 1
        nLookIndex           <- 1 
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
        
        for( i in 1:NumTreatments )
        {
            # It is a fixed sample design, so no interim look nor futility check.
            bFAEfficacyCheck <- TRUE # If TRUE, declares efficacy.
            # Usually, bFAEfficacyCheck would be a conditional statement such as 'dTValue > dBoundary'.
            # This would be different for each treatment arm.
        
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                       bFAEfficacyCondition = bFAEfficacyCheck)

            nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
            vDecision[ i ] = nDecision
        }
        
        
        return( list( Decision = as.integer( vDecision ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    PerformDecision <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        library( CyneRgy )
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vDecision           <- rep( 0, NumTreatments )  # Initializing decision vector to 0
        
        # This is an example using GetDecisionString and GetDecision.
        # Decision should take care of multiple comparison procedures adjustment appropriately.
        # Write the actual code here.
        
        # These variables are from LookInfo because it is a group sequential design.
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
        
        for( i in 1:NumTreatments )
        {
            # Write the actual code here.
            # It is a group sequential design, so interim looks and futility check are possible.
            bIAEfficacyCheck <- TRUE # If TRUE, declares efficacy at the interim look.
            bIAFutilityCheck <- FALSE # If TRUE, declares futility at the interim look.
            bFAEfficacyCheck <- TRUE # If TRUE, declares efficacy at the final look.
            # Usually, the Check variables would be conditional statements such as 'dTValue > dBoundary'.
            # This would be different for each treatment arm.
        
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                       bIAEfficacyCondition = bIAEfficacyCheck,
                                                       bIAFutilityCondition = bIAFutilityCheck,
                                                       bFAEfficacyCondition = bFAEfficacyCheck )

            nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
            vDecision[ i ] = nDecision
        }
        
        
        return( list( Decision = as.integer( vDecision ), ErrorCode = as.integer( nError ) ) )
    }

#### Minimal Template for Option 2 (TestStat)

##### For `Statistical Design = Fixed Sample`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vTestStatistic      <- rep( 0, NumTreatments )  # Initializing test statistic vector to 0   
        
        # Write the actual code here.
        # Store the computed test statistic for each treatment arm in vTestStatistic.
        
        return( list( TestStat = as.double( vTestStatistic ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vTestStatistic      <- rep( 0, NumTreatments )  # Initializing test statistic vector to 0   
        dDelta              <- 0 # Use if futility boundary scale is Delta
        
        # Write the actual code here.
        # Store the computed test statistic for each treatment arm in vTestStatistic.
        
        return( list( TestStat = as.double( vTestStatistic ),
                      Delta = as.double( dDelta ), # Include if futility boundary scale is Delta
                      ErrorCode = as.integer( nError ) ) )
    }

#### Minimal Template for Option 3 (AdjPVal)

##### For `Statistical Design = Fixed Sample`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vAdjPVal            <- rep( 0, NumTreatments )  # Initializing p-value vector to 0  
        
        # Write the actual code here.
        # Store the computed adjusted p-value for each treatment arm in vAdjPVal.
        
        return( list( AdjPVal = as.double( vTestStatistic ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vAdjPVal            <- rep( 0, NumTreatments )  # Initializing p-value vector to 0  
        dDelta              <- 0 # Use if futility boundary scale is Delta
        
        # Write the actual code here.
        # Store the computed adjusted p-value for each treatment arm in vAdjPVal.
        
        return( list( AdjPVal = as.double( vTestStatistic ),
                      Delta = as.double( dDelta ), # Include if futility boundary scale is Delta
                      ErrorCode = as.integer( nError ) ) )
    }

#### Minimal Template for Option 4 (RawPVal)

##### For `Statistical Design = Fixed Sample`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vRawPVal            <- rep( 0, NumTreatments )  # Initializing p-value vector to 0  
        
        # Write the actual code here.
        # Store the computed p-value for each treatment arm in vRawPVal.
        
        return( list( RawPVal = as.double( vTestStatistic ), ErrorCode = as.integer( nError ) ) )
    }

##### For `Statistical Design = Group Sequential`

    ComputeTestStat <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    {
        nError              <- 0 # Error handling (no error)
        NumTreatments       <- DesignParam$NumTreatments
        vRawPVal            <- rep( 0, NumTreatments )  # Initializing p-value vector to 0
        dDelta              <- 0 # Use if futility boundary scale is Delta
        
        # Write the actual code here.
        # Store the computed p-value for each treatment arm in vRawPVal.
        
        return( list( RawPVal = as.double( vTestStatistic ),
                      Delta = as.double( dDelta ), # Include if futility boundary scale is Delta
                      ErrorCode = as.integer( nError ) ) )
    }

## Examples

Explore the following examples for more context:

1.  [**2-Arm, Binary Outcome -
    Analysis**](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmBinaryOutcomeAnalysis.md)
    - [AnalyzeUsingEastManualFormulaNormal.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomeAnalysis/R/AnalyzeUsingEastManualFormula.R)
    - [AnalyzeUsingBetaBinomial.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomeAnalysis/R/AnalyzeUsingBetaBinomial.R)
    - [AnalyzeUsingPropLimitsOfCI.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomeAnalysis/R/AnalyzeUsingPropLimitsOfCI.R)
    - [AnalyzeUsingPropTest.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomeAnalysis/R/AnalyzeUsingPropTest.R)
    - [AnalyzeBinarySSR.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/2ArmBinaryOutcomeAnalysis/R/AnalyzeBinarySSR.R)
2.  [**Multiple Arm,
    Analysis**](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmAnalysis.md)
    - [AnalyzeMultiArmUsingPropTestBonferroni.R](https://github.com/Cytel-Inc/CyneRgy/blob/main/inst/Examples/MultiArmAnalysis/R/AnalyzeMultiArmUsingPropTestBonferroni.R)
