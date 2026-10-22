######################################################################################################################## .
#' @name AnalyzeMultiArmUsingPropTestBonferroni
#' @title Analyze binary outcome for multi-arm design using the prop.test function in the stats package in R.
#' @description Analyze binary outcome for multi-arm design using the prop.test function in the stats package in R. Use the
#'   documented inputs and outputs to integrate this function with the simulation workflow.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed.}
#'   \item{NumTreatments}{Integer number of experimental treatment arms, excluding control.}
#'   \item{IsArmPresent}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumCompleters}{Integer vector of cumulative completer counts, one per look.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#' }
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore). Return one value per experimental arm in treatment-ID order.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details Return Decision to apply custom stopping logic, or TestStat, AdjPVal, or RawPVal to let East Horizon
#'   apply its supported multiplicity adjustments and boundaries. Return one value per experimental arm
#'   in treatment-ID order and NA for arms absent at the current look. Delta is required for Delta-scale
#'   futility; HR is required for hazard-ratio-scale futility in time-to-event designs. RawPVal cannot be
#'   used for adjusted-p-value-scale futility. Return TestStat alongside RawPVal when the selected
#'   Dunnett procedure requires it. Use OutList to pass custom state between looks. This example may use
#'   only a subset of the documented design fields.
######################################################################################################################## .

AnalyzeMultiArmUsingPropTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL, OutList = NULL ) {

    # Step 1: Retrieve necessary information from the objects East Horizon sent ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        vInfoFrac <- LookInfo$InfoFrac
        vEfficacyBoundary <- gsDesign::gsDesign(
            k = nQtyOfLooks, test.type = 1, alpha = DesignParam$Alpha,
            sfu = gsDesign::sfLDOF, timing = vInfoFrac
        )
        vEfficacyBoundaryPScale <- 1 - stats::pnorm( vEfficacyBoundary$upper$bound )
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        vInfoFrac <- 1
        vEfficacyBoundaryPScale <- DesignParam$Alpha
    }

    vIsTrtPresent <- DesignParam$IsArmPresent
    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vector of data for control
    vOutcomesS <- vPatientOutcome[ vPatientTreatment == 0 ]

    # Calculate p-value for each hypothesis. Return NA if arm not present in the current analysis
    vPValues <- rep( NA, DesignParam$NumTreatments )
    for ( nTrtID in 1:DesignParam$NumTreatments ) {
        if ( vIsTrtPresent[ nTrtID ] == 1 ) {
            vOutcomesE <- vPatientOutcome[ vPatientTreatment == nTrtID ]
            mData <- rbind(
                c( sum( vOutcomesE ), length( vOutcomesE ) - sum( vOutcomesE ) ),
                c( sum( vOutcomesS ), length( vOutcomesS ) - sum( vOutcomesS ) )
            )
            lAnalysisResult <- stats::prop.test( mData, alternative = "greater", correct = FALSE )
            dPValue <- lAnalysisResult$p.value
        } else {
            dPValue <- NA
        }
        vPValues[ nTrtID ] <- dPValue
    }

    # Calculate Bonferroni adjusted p values
    vAdjPValues <- vPValues * sum( vIsTrtPresent )

    # Perform the desired analysis. NA should be returned for arms that are not available at this look
    # vDecision                    <- ifelse( vAdjPValues < vEfficacyBoundaryPScale[ nLookIndex], 2, 0 )  # A decision of 2 means success, 0 means continue the trial
    vDecision <- c( )
    for ( i in 1:length( vAdjPValues ) ) {
        if ( vIsTrtPresent[ i ] == 1 ) {
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                bIAEfficacyCondition = vAdjPValues[ i ] < vEfficacyBoundaryPScale[ nLookIndex ],
                bFAEfficacyCondition = vAdjPValues[ i ] < vEfficacyBoundaryPScale[ nLookIndex ]
            )
            nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
        } else {
            nDecision <- NA_integer_
        }
        vDecision <- c( vDecision, nDecision )
    }
    # for( i in 1:length(vDecision) ){
    #     if( vDecision[i] == 0 )
    #     {
    #         # Did not hit efficacy, so check futility
    #         # We are at the FA, efficacy decision was not made yet so the decision is futility
    #         if( nLookIndex == nQtyOfLooks )
    #         {
    #             vDecision[i]     <- 3 # Code for futility
    #         }
    #     }
    # }

    nErrorCode <- 0

    return( list(
        Decision = as.integer( vDecision ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}
