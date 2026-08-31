######################################################################################################################## .
#' @name AnalyzeUsingMMRM
#' @title Perform MMRM analysis
#' @description
#' Fits a mixed model for repeated measures to simulated patient data and returns
#' the treatment effect estimate, p-value, and East Horizon decision.
#' @author Jacob Wathen
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. It will have headers indicating the names of the
#' columns. These names will be same as those used in Data Generation. For analysis the most relevant variables are:
#'        \describe{
#'          \item{ArrivalTime}{Numeric vector representing patient arrival times}
#'          \item{TreatmentID}{Integer vector (0 = control, 1 = treatment)}
#'          \item{Response[X]}{Numeric vector representing response for visit X, where X = 1, 2, 3, 4, 5}
#'         }
#' @param DesignParam List which consists of Design and Simulation Parameters which user may need to compute
#' test statistic and perform test. For analysis the most relevant variable is:
#'        \describe{
#'          \item{Alpha}{1-sided Type I Error}
#'        }
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
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
#'         \item{PrimDelta}{Estimated treatment effect from the MMRM model at the final visit.}
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

AnalyzeUsingMMRM <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
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

    # Step 3: Fit the MMRM with default CS + varIdent ####
    lmeCtrls <- nlme::lmeControl( opt         = "optim",
                                  optimMethod = "BFGS",
                                  optimCtrl   = list( maxit = 500 ),
                                  nlminb      = list( iter.max = 100, eval.max = 200 ) )

    # Make last visit the reference level
    strLastVisit <- tail( levels( dfNoBaselineAnalysisData$Visit ), 1 )

    dfNoBaselineAnalysisData$Visit <- relevel( dfNoBaselineAnalysisData$Visit, ref = strLastVisit )

    mmrmModel <- tryCatch(
    {
        nlme::lme( Response ~ Baseline + TreatmentID * Visit,
               random      = ~ 1 | Id,
               correlation = nlme::corCompSymm( form = ~ 1 | Id ),
               weights     = nlme::varIdent( form = ~ 1 | Visit ),
               data        = dfNoBaselineAnalysisData,
               method      = "REML",
               na.action   = stats::na.omit,
               control     = lmeCtrls )

    }, error = function( e )
    {
        # Non-fatal error should skip the simulation if it does not work
        nError <- 0
        NULL
    } )

    # Step 3b: Extract the treatment × last‐visit effect ####
    if( !is.null( mmrmModel ) )
    {
        tTable <- summary( mmrmModel )$tTable
        cTrtRow <- grep( "^TreatmentID", rownames( tTable ), value = TRUE )[ 1 ]

        if( cTrtRow %in% rownames( tTable ) )
        {
            dPrimDelta <- tTable[ cTrtRow, "Value" ]
            stdErr     <- tTable[ cTrtRow, "Std.Error" ]
            df         <- tTable[ cTrtRow, "DF" ]
            dPValue    <- max( tTable[ cTrtRow, "p-value" ], .Machine$double.eps )
        }
        else
        {
            warning( "Interaction term not found." )
            dPValue <- 1.0
        }
    }
    else
    {
        dPValue <- 1.0
        nError  <- 0
    }

    # Step 4: Obtain group‐sequential alpha ####
    if( !is.null( LookInfo ) )
    {
        gsDesign     <- rpact::getDesignGroupSequential( kMax         = nQtyOfLooks,
                                                         alpha        = DesignParam$Alpha,
                                                         sided        = 1,
                                                         typeOfDesign = "OF" )
        dAlpha <- gsDesign$alphaSpent[ nLookIndex ]
    }
    else
    {
        dAlpha <- DesignParam$Alpha
    }

    # Step 5: Decision rules ####

    if( dPValue <= dAlpha )
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
        strDecision <- CyneRgy::GetDecisionString( LookInfo = LookInfo,
                                                   nLookIndex = nLookIndex,
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
                  PrimDelta = as.double( dPrimDelta ),
                  p.value   = as.double( dPValue ),
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
    # The dataset removes the baseline visit from the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit != 1 )
    dfBaselineAnalysisData   <- dplyr::filter( dfAnalysisData, Visit == 1 ) |>
        dplyr::select( Id, Baseline = Response )
    dfNoBaselineAnalysisData <- dplyr::left_join( dfNoBaselineAnalysisData, dfBaselineAnalysisData, by = "Id" )

    return( dfNoBaselineAnalysisData )
}
