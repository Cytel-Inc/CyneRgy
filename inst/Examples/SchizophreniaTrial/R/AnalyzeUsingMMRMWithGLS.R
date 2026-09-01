######################################################################################################################## .
#' @name AnalyzeUsingMMRMWithGLS
#' @title Perform MMRM Analysis Using Generalized Least Squares (GLS)
#' @description
#' Fits a mixed model for repeated measures using generalized least squares and
#' returns the treatment effect estimate, p-value, and East Horizon decision.
#' @author Jacob Wathen
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{Numeric vector representing patient arrival times}
#'          \item{TreatmentID}{Integer vector (0 = control, 1 = treatment)}
#'          \item{Response[X]}{Numeric vector representing response for visit X, where X = 1, 2, 3, 4, 5}
#'         }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'        \describe{
#'          \item{Alpha}{1-sided Type I Error}
#'        }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumCompleters}{Cumulative number of completer for all non time-to-event studies.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale.  Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale. Possible value are:  Z Scale: 0, p-Value Scale: 1, Delta Scale: 2, Conditional Power Scale: 3}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                      \item{InterimVisit}{1 based index of the visit which is driving the interims}
#'                      \item{FutContrast}{The contrast based on which futility boundaries are being computed. 0- Primary, 1-Secondary}
#'                      \item{IncludePipeline}{Flag indicating whether to include pipeline subjects in the interim or not. 0- Don't include. 1- Include}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @return A list containing the following elements:
#'         \describe{
#'           \item{Decision}{Optional integer value indicating the decision:
#'                           \describe{
#'                             \item{0}{No boundary crossed (neither efficacy nor futility).}
#'                             \item{1}{Lower efficacy boundary crossed.}
#'                             \item{2}{Upper efficacy boundary crossed.}
#'                             \item{3}{Futility boundary crossed.}
#'                             \item{4}{Equivalence boundary crossed.}
#'                           }}
#'         \item{PrimDelta}{Estimated treatment effect from the MMRM model with GLS at the final visit.}
#'         \item{p.value}{P-value for the analysis.}
#'         \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }}
#'         }
#' @details
#' ## CyneRgy Decision Helpers
#'
#' The analysis may use `CyneRgy::GetDecisionString()` and
#' `CyneRgy::GetDecision()` to determine the decision returned to
#' East Horizon Explore.
#'
#' When these helpers are used, the following input fields are required
#' and MUST be included when generating sample/test data:
#'
#' DesignParam:
#'   - TailType: Integer indicating the direction of the statistical test.
#'       0 = Left-tailed
#'       1 = Right-tailed
#'
#' LookInfo (for group sequential designs, NULL for fixed designs):
#' When not NULL, must contain the following fields:
#'   - NumLooks: Total number of looks.
#'   - CurrLookIndex: Current look index, starting at 1.
#'   - RejType: Integer identifying which stopping boundaries are enabled.
#'       0 = 1-Sided Efficacy Upper
#'       1 = 1-Sided Futility Upper
#'       2 = 1-Sided Efficacy Lower
#'       3 = 1-Sided Futility Lower
#'       4 = 1-Sided Efficacy Upper and Futility Lower
#'       5 = 1-Sided Efficacy Lower and Futility Upper
#'       6 = 2-Sided Efficacy Only (not used in East Horizon Explore)
#'       7 = 2-Sided Futility Only (not used in East Horizon Explore)
#'       8 = 2-Sided Efficacy and Futility (not used in East Horizon Explore)
#'       9 = Equivalence (not used in East Horizon Explore)
#'
######################################################################################################################## .

AnalyzeUsingMMRMWithGLS <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Initialize outputs
    nError     <- 0
    nDecision  <- 0

    # Step 1: Setup LooksInfo ####
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit       <- LookInfo$InterimVisit
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }

    # Step 2: Create analysis dataset ####
    dfNoBaselineAnalysisData <- CreateAnalysisDataset( SimData, LookInfo )

    # Step 3: Fit the MMRM using nlme::gls ####
    nVisits <- sum( grepl( "^Response", names( SimData ) ) )
    dfNoBaselineAnalysisData$TreatmentID <- as.factor( dfNoBaselineAnalysisData$TreatmentID )
    dfNoBaselineAnalysisData$Visit       <- factor( dfNoBaselineAnalysisData$Visit, levels = seq( 2 : nVisits ) )

    # Create the vectors for analysis, using the names needed for the GetLSDiffGLS
    vOut      <- dfNoBaselineAnalysisData$Response
    vBaseline <- dfNoBaselineAnalysisData$Baseline
    vTrt      <- dfNoBaselineAnalysisData$TreatmentID
    vTime     <- dfNoBaselineAnalysisData$Visit
    vIND      <- dfNoBaselineAnalysisData$Id

    glsFit <- nlme::gls( vOut ~ vBaseline + vTrt * vTime,
                         weights = nlme::varIdent( form = ~ 1 | vTime ),
                         correlation = nlme::corSymm( form = ~ 1 | vIND ),
                         na.action = stats::na.omit )

    lRetGLS <- GetLSDiffGLS( glsFit, 1, nVisits, FALSE )

    # Step 4: Obtain group‐sequential alpha ####
    if( !is.null( LookInfo ) )
    {
        gsDesign <- rpact::getDesignGroupSequential( kMax         = nQtyOfLooks,
                                 alpha        = DesignParam$Alpha,
                                 sided        = 1,
                                 typeOfDesign = "OF" )

        dAlpha   <- gsDesign$alphaSpent[ nLookIndex ]
    }
    else
    {
        dAlpha   <- DesignParam$Alpha
    }

    # Step 5: Decision rules ####
    if( lRetGLS$dPValue <= dAlpha )
    {
        if( nLookIndex == nQtyOfLooks )
        {
            # FA Efficacy condition
            bIAEfficacyCondition <- FALSE
            bFAEfficacyCondition <- TRUE
        }
        else
        {
            # IA Efficacy condition
            bIAEfficacyCondition <- TRUE
            bFAEfficacyCondition <- FALSE
        }
        # Efficacy decision
        strDecision <- CyneRgy::GetDecisionString( LookInfo   = LookInfo,
                                                   nLookIndex  = nLookIndex,
                                                   nQtyOfLooks = nQtyOfLooks,
                                                   bIAEfficacyCondition = bIAEfficacyCondition,
                                                   bFAEfficacyCondition = bFAEfficacyCondition )

        nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }
    else
    {
        strDecision <- CyneRgy::GetDecisionString( LookInfo = LookInfo,
                                                   nLookIndex = nLookIndex,
                                                   nQtyOfLooks = nQtyOfLooks )

        nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }

    # Step 6: Return analysis results ####
    lRet <- list( Decision  = as.integer( nDecision ),
                  PrimDelta = as.double( lRetGLS$dEs ),
                  p.value   = as.double( lRetGLS$dPValue ),
                  ErrorCode = as.integer( nError ) )

    return( lRet )
}
# Create a dataset for analysis ####

CreateAnalysisDataset <- function( SimData, LookInfo )
{
    # Step 1: Setup LooksInfo ####
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit       <- LookInfo$InterimVisit
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }

    # Step 2: Reshape wide → long in one shot ####
    dfLongData <- SimData |>
        dplyr::mutate( Id = dplyr::row_number() ) |>

        tidyr::pivot_longer( cols          = tidyselect::matches( "^(Response|ArrTimeVisit)\\d+$" ),
                             names_to      = c( ".value", "Visit" ),
                             names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) |>

        dplyr::mutate( Visit             = as.integer( Visit ),
                       CalendarVisitTime = ArrivalTime + ArrTimeVisit ) |>

        dplyr::select( Id, TreatmentID, Visit, Response, CalendarVisitTime ) |>

        dplyr::arrange( Visit, CalendarVisitTime )

    # Step 3: Interim‐look filtering using dplyr ####
    if( !is.null( LookInfo ) )
    {

        # 3a) compute cutoff time
        dAnalysisTime <- dfLongData |>
            dplyr::filter( Visit == nAnalysisVisit ) |>
            dplyr::slice( nQtyOfPatsForInterim ) |>
            dplyr::pull( CalendarVisitTime )

        # 3b) pick subjects
        if( LookInfo$IncludePipeline == 0 )
        {
            vSubjectsForAnalysis <- dfLongData |>
                dplyr::filter( Visit == nAnalysisVisit,
                               CalendarVisitTime <= dAnalysisTime ) |>
                dplyr::distinct( Id ) |>
                dplyr::pull( Id )
        }
        else
        {
            vSubjectsForAnalysis <- dfLongData |>
                dplyr::filter( CalendarVisitTime <= dAnalysisTime ) |>
                dplyr::distinct( Id ) |>
                dplyr::pull( Id )
        }

        dfAnalysisData <- dfLongData |>
            dplyr::filter( Id %in% vSubjectsForAnalysis )
    }
    else
    {
        dfAnalysisData <- dfLongData
    }

    # Step 4: Prepare for MMRM ####
    dfAnalysisData <- dfAnalysisData |>
        dplyr::mutate( Visit       = factor( Visit ),
                       TreatmentID = factor( TreatmentID ),
                       Id          = factor( Id ) )

    # Step 5: Create a dataset ####
    # The dataset removes the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit != 1 )
    dfBaselineAnalysisData   <- dplyr::filter( dfAnalysisData, Visit == 1 ) |>
        dplyr::select( Id, Baseline = Response )
    dfNoBaselineAnalysisData <- dplyr::left_join( dfNoBaselineAnalysisData, dfBaselineAnalysisData, by = "Id" )

    return( dfNoBaselineAnalysisData )
}

# Function to compute Least Squares Difference from GLS Fit

GetLSDiffGLS <- function( glsFit, nTrt, nTime, bPlacMinusTrt )
{
    # Step 1: Construct variable names for treatment, time and intercept ####
    strWhichTrt     <- paste( "vTrt" , nTrt, sep = "" )
    strWhichTime    <- paste( "vTime", nTime, sep = "" )
    strIntercept    <- "(Intercept)"

    # Step 2: Determine which variables to include in the estimate ####
    vCoeff          <- stats::coef( glsFit )

    if( !any( names( vCoeff ) == strWhichTime ) )
    {
        # Time is the baseline; no need to include the interaction
        vVarNames       <- strWhichTrt
        vVarNamesPlac   <- strIntercept
        vVarNamesTrt    <- c( strIntercept, strWhichTrt )
    }
    else
    {
        # Time is not baseline; include trt * time interaction
        vVarNames       <- c( strWhichTrt, paste( strWhichTrt, strWhichTime, sep = ":" ) )
        vVarNamesPlac   <- c( strIntercept, strWhichTime )
        vVarNamesTrt    <- c( vVarNamesPlac, vVarNames )
    }

    # Include baseline covariate if present
    if( any( names( vCoeff ) == "vBaseline" ) )
    {
        vVarNamesPlac <- c( vVarNamesPlac, "vBaseline" )
        vVarNamesTrt  <- c( vVarNamesTrt, "vBaseline" )
    }

    # Step 3: Degrees of freedom ####
    nDOF <- diff( unlist( glsFit$dims ) [ 2:1 ] )

    # Step 4: Estimate for Treatment - Placebo ####
    dEst <- sum( vCoeff[ vVarNames ] )
    if( bPlacMinusTrt ) dEst <- dEst * -1

    # Step 5: Compute standard error, t-statistic, and p-value ####
    dSE      <- sqrt( sum( stats::vcov( glsFit )[ vVarNames, vVarNames ] ) )
    dTStat   <- dEst / dSE
    dPValue  <- stats::pt( dTStat, nDOF )

    # Step 6: Final return ####
    lRet <- list( dPValue = dPValue, dEst = dEst, nDOF = nDOF, dSE = dSE, dTStat = dTStat )

    return( lRet )
}
