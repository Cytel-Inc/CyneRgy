######################################################################################################################## .
#' @name AnalyzeDoseFindingContinuousUsingPairwiseTTest
#'
#' @title Analyze continuous outcome for dose finding design using Fixed Sequence Pairwise t-test.
#'
#' @description This function implements Fixed Sequence Pairwise testing for dose-finding studies with continuous
#'   endpoints. The Fixed Sequence gatekeeping procedure tests hypotheses sequentially from the highest dose
#'   downward, rejecting the null hypothesis only if the raw p-value is less than the total alpha. Once a
#'   hypothesis fails to reject, all lower dose hypotheses are automatically rejected (futility cascade).
#'
#' @author Sayantan Biswas, Pradip Maske, Gabriel Potvin
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{CensorInd}{Integer vector of censor indicators, with one element per subject: 0 = dropout/non-completer;
#'     1 = completer.}
#'   \item{CensorIndOrg}{Original integer vector of censor indicators before any analysis-time adjustment: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{ClndrRespTime}{Numeric vector of response times on the calendar scale, with one element per subject. For
#'     a survival endpoint, this is ArrivalTime plus the event or censoring time measured from enrollment.}
#' }
#'
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TrialType}{Integer. Trial Type: – `0`: Superiority.}
#'   \item{TestType}{Integer. Test Type: – `0`: One-sided.}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed.}
#'   \item{InitialAllocInfo}{Vector of Numeric. Vector of length equal to the number of treatment arms (number of
#'     arms - 1), containing the ratios of the treatment group sample sizes to control group sample size.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#'   \item{TestStatType}{Integer. Test statistic type: - `3`: Z-test. - `4`: t-test. East Horizon Explore: Only
#'     available for `Endpoint Type = Continuous`. East Horizon Design: Only available for `Continuous` tests.}
#'   \item{VarType}{Integer. Variance type. For `Continuous` test: - `4`: Equal - `5`: Unequal. For `Difference of
#'     Proportions` (Binary) test: - `0`: Pooled. `1`: Unpooled. East Horizon Explore: Not available for `Endpoint
#'     Type = Time-to-Event`. East Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{Sigma}{Numeric. Design standard deviation specified in simulations. East Horizon Explore: Not available.
#'     East Horizon Design: Only available for `Test = MAMS Difference of Means: Combining P-Values` (Continuous)
#'     and `Test Stat Type = 3 (Z-test)`.}
#'   \item{PValCombMethod}{Integer. P-value combination method: - `0`: Inverse normal. East Horizon Explore: Not
#'     available. East Horizon Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w1}{Numeric. Inverse normal weights for stage 1. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w2}{Numeric. Inverse normal weights for stage 2. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{MultAdjMethod}{Integer. Multiple comparison procedure. East Horizon Explore: Possible values: – `0`:
#'     Bonferroni. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. East Horizon Design: Possible values:- – `0`: Bonferroni. – `1`: Sidak.
#'     – `2`: Simes. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. – `10`: Holm's Step Down. – `11`: Hommel's Step Up. – `12`: Dunnett's
#'     Step Down. – `13`: Dunnett's Step Up.}
#'   \item{NumTreatments}{Integer number of experimental treatment arms, excluding control.}
#'   \item{IsArmPresent}{Vector or Integer.. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#'   \item{UpdatedAllocInfo}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the updated ratios of the treatment group sample sizes to control group sample size, which may
#'     have been updated during treatment selection.}
#' }
#'
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumAlpha}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the cumulative alpha spent
#'     (for one-sided tests) for each look. East Horizon Design: Only available if `Test Type = One-sided`.}
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number
#'     of completers for each look. East Horizon Explore: Not available for `Endpoint Type = Time-to-Event`. East
#'     Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{RejType}{Integer. Rejection type: – `0`: One-sided efficacy upper. – `1`: One-sided futility upper. –
#'     `2`: One-sided efficacy lower. – `3`: One-sided futility lower. – `4`: One-sided efficacy upper, futility
#'     lower. – `5`: One-sided efficacy lower, futility upper.}
#'   \item{FutBdryScale}{Integer. Futility boundary scale. East Horizon Explore: Possible values: – `2`: Delta
#'     scale. – `6`: Hazard ratio scale. East Horizon Design: Possible values: – `1`: Adjusted p-value scale. –
#'     `2`: Delta scale. – `6`: Hazard ratio scale.}
#'   \item{FutBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the futility boundary
#'     values for each look.}
#'   \item{PoCScale}{Integer. Proof of concept (PoC) scale: - `0`: High dose vs. control. East Horizon Explore:
#'     Only available for `Study Objective = Dose Finding`. East Horizon Design: Not available.}
#'   \item{PoCThreshold}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the proof of concept
#'     (PoC) threshold. East Horizon Explore: Only available for `Study Objective = Dose Finding`. East Horizon
#'     Design: Not available.}
#' }
#'
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore). Return one value per experimental arm in treatment-ID order.}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale. Return one value per experimental arm in
#'     treatment-ID order.}
#'   \item{Delta}{Numeric vector of estimated experimental-minus-control treatment effects, one per experimental
#'     arm in treatment-ID order. For binary outcomes, each effect is a proportion difference; for continuous
#'     outcomes, each effect is a mean difference.}
#'   \item{CtrlCompleters}{Number of completers in the control arm. Required when the selected conditional-power
#'     rule uses the estimated treatment effect.}
#'   \item{TrmtCompleters}{Number of completers in the experimental arm. Required when the selected
#'     conditional-power rule uses the estimated treatment effect.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{AdjPVal}{Numeric vector of p-values adjusted for multiple comparisons, one per experimental arm.}
#'   \item{RawPVal}{Numeric vector of unadjusted p-values, one per experimental arm.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#'   \item{POCStatusArm}{Optional integer vector indicating proof-of-concept status for each experimental arm: 0 =
#'     threshold not crossed; 1 = threshold crossed. Available for dose-finding designs.}
#'   \item{POCStatus}{Optional integer overall proof-of-concept status: 0 = threshold not crossed; 1 = threshold
#'     crossed. This example uses the highest-dose arm to determine the overall status.}
#' }
######################################################################################################################## .

AnalyzeDoseFindingContinuousUsingPairwiseTTest <- function( SimData, DesignParam, LookInfo, OutList, UserParam = NULL ) {
    nErrorCode <- 0L

    # Reading inputs ####
    # Extracting parameters from DesignParam
    nNumTrt <- DesignParam$NumTreatments
    dTotalAlpha <- DesignParam$Alpha
    nTailType <- DesignParam$TailType
    nVarType <- DesignParam$VarType
    bIsArmPresent <- DesignParam$IsArmPresent
    # Extracting parameters from LookInfo
    nNumLooks <- LookInfo$NumLooks
    nCurrLookIndex <- LookInfo$CurrLookIndex
    vFutBdry <- LookInfo$FutBdry
    dPoCThreshold <- LookInfo$PoCThreshold
    vCumCompleters <- LookInfo$CumCompleters

    # Initializing output vectors ####
    # Decision vector: NA = dropped, 0 = no boundary, 1 = lower eff, 2 = upper eff, 3 = futility
    vDecision <- rep( 0L, nNumTrt )
    vRawPVal <- rep( NA_real_, nNumTrt )
    vTestStat <- rep( NA_real_, nNumTrt )
    vDelta <- rep( NA_real_, nNumTrt )
    vIsoDelta <- rep( NA_real_, nNumTrt )
    # PoC status vectors
    vPOCStatusArm <- rep( 0.0, nNumTrt )
    dOverallPOC <- 0.0

    # Setting decision of earlier dropped arms/doses as NA
    vDecision[ bIsArmPresent == 0 ] <- NA_integer_
    # Reading PoC status of the last look
    if ( nCurrLookIndex > 1 ) {
        vPOCStatusArm <- OutList$vPOCStatusArm
        dOverallPOC <- OutList$dOverallPOC
    }

    # Compute Analysis Time ####
    dEstAnalysisTime <- ComputeAnalysisTime(
        dRespLag = DesignParam$dRespLag, vArrivalTime = SimData$ArrivalTime,
        vCumCompleters = vCumCompleters, nCurrLookIndex = nCurrLookIndex
    )

    # Prepare data ###
    dfSimData <- SimData[ with( SimData, ClndrRespTime <= dEstAnalysisTime & CensorIndOrg == 1 ), ]

    # Compute Summary ####
    # Identify active and dropped arms
    vSelectedArmIndex <- which( bIsArmPresent == 1 )
    nNumActive <- length( vSelectedArmIndex )
    vDroppedArms <- which( bIsArmPresent == 0 )

    vRespCtrl <- dfSimData$Response[ dfSimData$TreatmentID == 0 ]
    nNumCtrl <- length( vRespCtrl )
    dMeanRespCtrl <- mean( vRespCtrl )
    dVarRespCtrl <- stats::var( vRespCtrl )
    vNumTrt <- rep( NA_real_, nNumActive )
    vMeanRespTrt <- rep( NA_real_, nNumActive )
    vVarRespTrt <- rep( NA_real_, nNumActive )
    vDeltaEst <- rep( NA_real_, nNumActive )

    for ( nArmNum in seq_along( vSelectedArmIndex ) ) {
        nTrtArmIndex <- vSelectedArmIndex[ nArmNum ]
        vRespTrt <- dfSimData$Response[ dfSimData$TreatmentID == nTrtArmIndex ]
        vNumTrt[ nArmNum ] <- length( vRespTrt )
        vMeanRespTrt[ nArmNum ] <- mean( vRespTrt )
        vVarRespTrt[ nArmNum ] <- stats::var( vRespTrt )
        vDeltaEst[ nArmNum ] <- vMeanRespTrt[ nArmNum ] - dMeanRespCtrl
    }
    # Compute isotonic deltas: enforce monotonicity on deltas
    vIsoDelta[ vSelectedArmIndex ] <- ComputeIsotonicDeltas( vDeltaEst, nTailType )

    # Proof of Concept Assessment ####
    if ( !is.null( dPoCThreshold ) && !is.na( dPoCThreshold ) ) {
        vActiveForPoC <- which( !is.na( vIsoDelta ) )
        if ( length( vActiveForPoC ) > 0 ) {
            # PoC: independent per-arm check for Pairwise test
            if ( nTailType == 1L ) {
                # Right-Tail: PoC if delta > threshold
                vArmWithPOC <- which( round( vIsoDelta[ vSelectedArmIndex ], 7 ) > dPoCThreshold )
            } else {
                # Left-Tail: PoC if delta < threshold
                vArmWithPOC <- which( round( vIsoDelta[ vSelectedArmIndex ], 7 ) < dPoCThreshold )
            }
            vPOCStatusArm[ vSelectedArmIndex[ vArmWithPOC ] ] <- 1

            # Overall PoC: determined by highest dose arm (i.e. last treatment arm)
            dOverallPOC <- ifelse( vPOCStatusArm[ max( vSelectedArmIndex ) ] == 1, 1, 0 )
        }
    }

    # Check for Futility in interims using isotonic delta ####
    bTestFutility <- !is.null( vFutBdry ) && !is.na( vFutBdry[ nCurrLookIndex ] ) && nCurrLookIndex < nNumLooks
    if ( bTestFutility ) {
        dFutBdryVal <- vFutBdry[ nCurrLookIndex ]

        # vDelta Scale: compare delta estimate against futility boundary
        if ( nTailType == 1L ) {
            vDecision[ vSelectedArmIndex ] <- as.numeric( vIsoDelta[ vSelectedArmIndex ] < dFutBdryVal ) * 3
        } else {
            vDecision[ vSelectedArmIndex ] <- as.numeric( vIsoDelta[ vSelectedArmIndex ] > dFutBdryVal ) * 3
        }
    }

    # Testing for efficacy at the last look ####
    if ( nCurrLookIndex == nNumLooks && nNumActive > 0 ) {
        # Compute variance and degrees of freedom
        if ( nVarType == 4L ) {
            # Equal variance: pooled
            vSp2 <- ( ( nNumCtrl - 1 ) * dVarRespCtrl + ( vNumTrt - 1 ) * vVarRespTrt ) / ( nNumCtrl + vNumTrt - 2 )
            vSE <- sqrt( vSp2 * ( 1 / nNumCtrl + 1 / vNumTrt ) )
            vDOF <- nNumCtrl + vNumTrt - 2
        } else {
            # Unequal variance: Welch
            dSEC <- dVarRespCtrl / nNumCtrl
            vSET <- vVarRespTrt / vNumTrt
            vSE <- sqrt( dSEC + vSET )
            vDOF <- round( ( dSEC + vSET )^2 / ( dSEC^2 / ( nNumCtrl - 1 ) + vSET^2 / ( vNumTrt - 1 ) ) )
        }

        # Compute test statistic and p-value
        vTestStat[ vSelectedArmIndex ] <- vIsoDelta[ vSelectedArmIndex ] / vSE

        if ( nTailType == 1L ) {
            # Right-Tail: P(T > dTstat)
            vRawPVal[ vSelectedArmIndex ] <- stats::pt( vTestStat[ vSelectedArmIndex ], df = vDOF, lower.tail = FALSE )
        } else {
            # Left-Tail: P(T < dTstat)
            vRawPVal[ vSelectedArmIndex ] <- stats::pt( vTestStat[ vSelectedArmIndex ], df = vDOF, lower.tail = TRUE )
        }

        lTestResults <- ApplyFixedSeqPairwiseTest(
            vRawPValues = vRawPVal[ vSelectedArmIndex ],
            vDoseSequence = nNumActive:1,
            dAlpha = dTotalAlpha
        )
        if ( nTailType == 0 ) {
            lTestResults$vDecision <- as.integer( lTestResults$vDecision * 1 )
        } else {
            lTestResults$vDecision <- as.integer( lTestResults$vDecision * 2 )
        }

        vDecision[ vSelectedArmIndex ] <- lTestResults$vDecision
        vDecision[ vDecision == 0 ] <- 3 # Setting all the non-concluded hypotheses as futile
    }

    # Prepare OutList for next look ####
    lNewOutList <- list( vPOCStatusArm = vPOCStatusArm, dOverallPOC = dOverallPOC )

    # Return Results ####
    return( list(
        Decision = as.integer( vDecision ),
        RawPVal = as.double( vRawPVal ),
        TestStat = as.double( vTestStat ),
        Delta = as.double( vIsoDelta ),
        POCStatusArm = as.integer( vPOCStatusArm ),
        POCStatus = as.integer( dOverallPOC ),
        AnalysisTime = as.double( dEstAnalysisTime ),
        OutList = lNewOutList,
        ErrorCode = as.integer( nErrorCode )
    ) )
}

######################################################################################################################## .
# HELPER FUNCTION: Analysis Time Computation ####
######################################################################################################################## .
ComputeAnalysisTime <- function( dRespLag, vArrivalTime, vCumCompleters, nCurrLookIndex ) {
    dRespLag <- ifelse( !is.null( dRespLag ), dRespLag, 0 )
    dCompletionTimes <- sort( vArrivalTime + dRespLag )
    nTargetCompleters <- vCumCompleters[ nCurrLookIndex ]

    if ( !is.null( nTargetCompleters ) &&
        !is.na( nTargetCompleters ) &&
        nTargetCompleters <= length( dCompletionTimes ) ) {
        dEstAnalysisTime <- dCompletionTimes[ nTargetCompleters ]
    } else {
        dEstAnalysisTime <- max( dCompletionTimes, na.rm = TRUE )
    }
    return( dEstAnalysisTime )
}

######################################################################################################################## .
# HELPER FUNCTION: Isotonic Regression via PAVA ####
# Computes isotonic regression to enforce monotonicity on values using
# the Pool Adjacent Violators Algorithm (PAVA).
#
# Inputs:
# numeric vector of values to be monotonized
# tail type:  0 = left-tail, 1 = right-tail
#
# Output:
# numeric vector of isotonic values (same length as input)
######################################################################################################################## .
ComputeIsotonicDeltas <- function( vValues, nTailType ) {
    n <- length( vValues )
    if ( n == 0 ) {
        return( numeric( 0 ) )
    }

    # For non-increasing (left-tail), negate values
    if ( nTailType == 0L ) {
        vValues <- -vValues
    }

    vBlockVal <- vValues
    vBlockIdx <- as.list( 1:n )

    repeat {
        bMerged <- FALSE
        i <- 1
        while ( i < length( vBlockVal ) ) {
            if ( vBlockVal[ i ] > vBlockVal[ i + 1 ] ) {
                # Merge blocks i and i+1 with simple average
                dNewVal <- mean( c( vBlockVal[ i ], vBlockVal[ i + 1 ] ) )
                vBlockVal[ i ] <- dNewVal
                vBlockVal <- vBlockVal[ -( i + 1 ) ]
                vBlockIdx[[ i ]] <- c( vBlockIdx[[ i ]], vBlockIdx[[ i + 1 ]] )
                vBlockIdx <- vBlockIdx[ -( i + 1 ) ]
                bMerged <- TRUE
            } else {
                i <- i + 1
            }
        }
        if ( !bMerged ) {
            break
        }
    }

    # Reconstruct result vector
    vResult <- numeric( n )
    for ( j in seq_along( vBlockVal ) ) {
        vResult[ vBlockIdx[[ j ]] ] <- vBlockVal[ j ]
    }

    # Negate back if left-tail
    if ( nTailType == 0L ) {
        vResult <- -vResult
    }

    return( vResult )
}

######################################################################################################################## .
# HELPER FUNCTION: Fixed Sequence Pairwise Test #####
# Example : MCP_Method = Fixed Sequence
#
# Inputs:
#   Vector of raw P values
#   Sequence of treatment arms by Dose (1: Highest Dose, n : Lowest Dose)
#   Alpha (Type I error)
# Output:
#   Vector of adjusted P values
#   global adjusted P-value
#   Decision
######################################################################################################################## .
ApplyFixedSeqPairwiseTest <- function( vRawPValues, vDoseSequence, dAlpha ) {
    # Input validation
    if ( length( vRawPValues ) != length( vDoseSequence ) ) {
        stop( "Length of vRawPValues and vDoseSequence must be equal" )
    }

    # Step 1: Order p-values according to sequence
    vOrdRawPValues <- vRawPValues[ order( vDoseSequence ) ]

    # Step 2: Compute adjusted p-values for each hypothesis
    vAdjPValues <- cummax( vOrdRawPValues )

    # Step 3: Compute adjusted p-value for global null hypothesis
    dAdjPValGlobal <- vOrdRawPValues[ 1 ]

    # Step 4: Rearrange adjusted p-values and decisions back to original order
    vIdxOrder <- order( vDoseSequence )
    vAdjPValues <- vAdjPValues[ order( vIdxOrder ) ]

    return( list(
        vAdjPValues = vAdjPValues,
        dAdjPValGlobal = dAdjPValGlobal,
        vDecision = ( vAdjPValues < dAlpha )
    ) )
}
