######################################################################################################################## .
#' @name AnalyzeSubpopulation
#' @title Analyze Stratified Time-to-Event Subpopulations
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#'
#' ASSUMPTION
#' The look positioning is based on target events on Full Population
#'
#' @param SimData
#' A data frame containing the simulated patient-level data for the current simulation iteration.
#' Includes at least the following variables:
#' \itemize{
#'   \item{ArrivalTime}{— The calendar time at which the subject entered the trial}
#'   \item{Response}{— The observed endpoint for continuous outcome}
#'   \item{TreatmentID}{— 0 = Control, 1 = Treatment}
#' }
#'
#' @param DesignParam
#' A list containing the design and simulation parameters required for analysis. Includes:
#' \itemize{
#'   \item{MaxCompleters}{— Maximum number of completers for the study}
#'   \item{RespLag}{— Response lag from arrival time to measurement}
#'   \item{CriticalPoint}{— Single-look efficacy boundary (if LookInfo = NULL)}
#'
#'   %% Stratification parameters
#'   \item{NumStratFactors}{— Number of stratification factors used in the analysis}
#'   \item{TestStratFactors}{— Subset of stratification factors to be used specifically for testing (may include \code{NA})}
#'   \item{StratFactors}{— A list of stratification factor levels, where each element corresponds
#'         to a stratification variable.
#'         For example:
#'         \itemize{
#'            \item{\code{Var1}}{— Levels for stratification variable 1 (e.g., \code{c("1","2")})}
#'            \item{\code{Var2}}{— Levels for stratification variable 2 (e.g., \code{c("1","2")})}
#'         }}
#' }
#'
#' %% Subpopulation Analysis Parameters
#'   \item{NumSubPops}{— Number of predefined subpopulations included in the analysis}
#'
#'   \item{SubpopName}{— A vector of subpopulation names or identifiers
#'         (e.g., \code{c("SP1","SP2","SP3")})}
#'
#'   \item{WinCond}{— A list specifying the win conditions for each subpopulation.
#'         Each element corresponds to a subpopulation and defines the criteria
#'         used to determine whether a treatment arm “wins” within that group.
#'         For example:
#'         \itemize{
#'            \item{\code{SP1}}{— Win condition settings for Subpopulation 1}
#'            \item{\code{SP2}}{— Win condition settings for Subpopulation 2}
#'            \item{\code{SP3}}{— Win condition settings for Subpopulation 3}
#'         }}
#'
#'   \item{PlanEndTrial}{— A logical flag or condition vector indicating whether
#'         the trial should be considered complete for each subpopulation at
#'         the planned analysis points (e.g., \code{TRUE} / \code{FALSE})}
#'
#'   \item{TransitionMatrix}{— A transition matrix or list of matrices defining
#'         how probabilities or subjects transition between states or
#'         subpopulations (if applicable).
#'         For example:
#'         \itemize{
#'            \item{\code{SP1}}{— Transition matrix for Subpopulation 1}
#'            \item{\code{SP2}}{— Transition matrix for Subpopulation 2}
#'            \item{\code{SP3}}{— Transition matrix for Subpopulation 3}
#'         }}
#' }
#'
#' @param LookInfo
#' A list containing group sequential design information for multi-look trials.
#' For group sequential designs, it includes:
#' \itemize{
#'   \item{NumLooks}{— Total number of interim analyses}
#'   \item{CurrLookIndex}{— Current look index}
#'   \item{InfoFrac}{— Information fraction at each look}
#'   \item{EffBdry}{— Efficacy boundary at each look}
#' }
#'
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' A list of user-defined parameters in East Horizon.  Default = NULL.
#'
#' @description
#' Computes teststat, hazard ratio, analysis time and decision
#' at a given interim analysis while having multiple subpopulations.
#'
#' This function:
#' \enumerate{
#'   \item Determines the number of events required at the current interim look
#'         (based on \code{LookInfo} if provided; otherwise on \code{DesignParam}).
#'
#'   \item Prepares the analysis dataset by:
#'         \itemize{
#'           \item Computing event times and observed follow-up
#'           \item Ordering subjects by event time to determine analysis cutoff
#'           \item Censoring subjects whose events occur after the analysis time
#'           \item Restricting subjects to those enrolled before the cutoff
#'         }
#'
#'   \item Reads all pre-specified population definitions:
#'         \itemize{
#'           \item Full population
#'           \item Subpopulations in \code{DesignParam$SubPops}
#'           \item Alpha allocation weights for GMCP
#'         }
#'
#'   \item Constructs logical filters that identify subjects belonging to the
#'         full population and each subpopulation.
#'
#'   \item Identifies all stratification factors used by any subpopulation and
#'         ensures they are appropriately factorized.
#'
#'   \item For each population (full population + all subpops), it:
#'         \itemize{
#'           \item Selects the applicable stratification factors
#'           \item Constructs a dynamic stratified log-rank formula
#'           \item Computes the standardized test statistic (sqrt of chi-square)
#'           \item Fits a stratified Cox model and extracts the hazard ratio (HR)
#'         }
#'
#'   \item Collects population-specific test statistics and applies the graphical
#'         multiple testing procedure via \code{compute_gMCPDecisions()}.
#'
#'   \item Converts GMCP rejection flags into population-specific decision codes:
#'         \itemize{
#'           \item \code{2} = reject null hypothesis (efficacy)
#'           \item \code{0} = continue at interim look
#'           \item \code{3} = no rejection at final look (futility)
#'         }
#'
#'   \item Returns all computed outputs for each population:
#'         \itemize{
#'           \item Test statistic
#'           \item Hazard ratio
#'           \item GMCP decision code
#'         }
#'
#'   \item Returns the overall analysis time at which the interim or final
#'         evaluation was conducted, along with an error flag.
#' }
#'
#' @return A named list containing population-specific `Decision`, `TestStat`, and `dHR` lists, numeric `AnalysisTime`, and integer `ErrorCode`.
#' }
######################################################################################################################## .

AnalyzeSubpopulation <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nError          <- 0
    dTimeOfAnalysis <- 0

    # Step 1: Determine number of events for analysis
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks  <- LookInfo$NumLooks
        nLookIndex   <- LookInfo$CurrLookIndex
        vCumEvents   <- LookInfo$CumEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Step 2: Prepare analysis dataset
    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis,
                                    dTimeOfAnalysis - SimData$ArrivalTime,
                                    SimData$TimeOfEvent - SimData$ArrivalTime )

    # Step 3: Read population inputs
    nNumSubPops <- DesignParam$NumSubPops
    vPopNames   <- DesignParam$SubpopName
    lSubPops    <- DesignParam$SubPops
    vPropAlpha  <- DesignParam$PropAlpha

    # Step 4: Create population filters
    lPopFilters <- list()
    lPopFilters[[ "Full Population" ] ] <- rep( TRUE, nrow( SimData ) )

    if( nNumSubPops > 0 )
    {
        for( strSubpopName in names( lSubPops ) )
        {
            vSubpopFilter <- rep( TRUE, nrow( SimData ) )
            for( strFactorName in names( lSubPops[[ strSubpopName ] ] ) )
            {
                vAllowedValues <- lSubPops[[ strSubpopName ] ][[ strFactorName ] ]
                vSubpopFilter  <- vSubpopFilter & ( SimData[[ strFactorName ] ] %in% vAllowedValues )
            }
            lPopFilters[[ strSubpopName ] ] <- vSubpopFilter
        }
    }

    # Step 5: Determine all possible factors across all subpopulations
    vAllFactors <- unique( unlist( lapply( lSubPops, names ) ) )
    for( strFactor in vAllFactors )
    {
        if( strFactor %in% names( SimData ) )
        {
            SimData[[ strFactor ] ] <- factor( SimData[[ strFactor ] ],
                                               levels = unique( SimData[[ strFactor ] ] ) )
        }
    }

    # Step 6: Initialize output lists
    lTestStatistics <- list()
    lHazardRatios   <- list()
    lDecisions      <- list()
    vTestStats      <- c()
    vPopOrder       <- c()

    # Step 7: Compute test statistics and collect populations
    for( strPopName in names( lPopFilters ) )
    {
        dfSubsetData <- SimData[ lPopFilters[[ strPopName ] ], ]

        if( nrow( dfSubsetData ) > 0 )
        {
            # Identify stratification factors
            if( strPopName == "Full Population" )
            {
                vCurrentStratFactors <- vAllFactors
            }
            else
            {
                vCurrentStratFactors <- names( lSubPops[[ strPopName ] ] )
            }
            vCurrentStratFactors <- vCurrentStratFactors[ vCurrentStratFactors %in% names( dfSubsetData ) ]

            # Build survival formula
            if( length( vCurrentStratFactors ) > 0 )
            {
                fStrataFormula <- stats::as.formula(
                    paste0(
                        "survival::Surv(ObservedTime, Event) ~ TreatmentID + ",
                        paste0( "survival::strata(`", vCurrentStratFactors, "`)", collapse = " + " )
                    )
                )
            }
            else
            {
                fStrataFormula <- survival::Surv( ObservedTime, Event ) ~ TreatmentID
            }

            # Estimate the hazard ratio
            cCoxFit <- survival::coxph( fStrataFormula, data = dfSubsetData )
            dHR     <- exp( coef( cCoxFit ) )

            # Run the log-rank test
            cSurvDiff <- survival::survdiff( fStrataFormula, data = dfSubsetData )
            dTestStat <- sqrt( cSurvDiff$chisq )
            dTestStat <- ifelse( unname( dHR ) < 1, dTestStat * -1, dTestStat )

            # Store outputs in named lists
            lTestStatistics[[ strPopName ] ] <- as.double( dTestStat )
            lHazardRatios[[ strPopName ] ]   <- as.double( dHR )
            vTestStats <- c( vTestStats, dTestStat )
            vPopOrder  <- c( vPopOrder, strPopName )
        }
        else
        {
            lTestStatistics[[ strPopName ] ] <- NA
            lHazardRatios[[ strPopName ] ]   <- NA
            vTestStats <- c( vTestStats, NA )
            vPopOrder  <- c( vPopOrder, strPopName )
        }
    }

    # Step 8: Compute GMCP decisions
    lGMCPResult <- ComputeGMCPDecisions(
        vTestStats  = vTestStats,
        nTailType   = DesignParam$TailType,
        dAlpha      = DesignParam$Alpha,
        vWeights    = DesignParam$PropAlpha,
        mTransition = DesignParam$TransitionMatrix
    )

    # Step 9: Map GMCP decisions to populations and store them in a named list
    for( iPop in seq_along( vPopOrder ) )
    {
        strPopName <- vPopOrder[ iPop ]
        nGMCPFlag  <- lGMCPResult$decisionFlag[ iPop ]

        if( nGMCPFlag == 1 )
        {
            nFinalDecision <- 2
        }
        else if( nLookIndex == nQtyOfLooks )
        {
            nFinalDecision <- 3
        }
        else
        {
            nFinalDecision <- 0
        }

        lDecisions[[ strPopName ] ] <- as.integer( nFinalDecision )
    }

    # Step 10: Return results
    lRet <- list( Decision     = as.list( lDecisions ),
                  TestStat     = as.list( lTestStatistics ),
                  dHR          = as.list( lHazardRatios ),
                  AnalysisTime = as.double( dTimeOfAnalysis ),
                  ErrorCode    = as.integer( nError ) )

    return( lRet )
}

ComputeGMCPDecisions <- function( vTestStats, nTailType, dAlpha, vWeights, mTransition )
{
    bTestStatMissing <- is.nan( vTestStats ) | is.na( vTestStats )
    if( any( bTestStatMissing ) )
    {
        vTestStats[ which( bTestStatMissing == TRUE ) ] <- ifelse( nTailType == "Left-Tail", Inf, -Inf )
    }

    # Compute raw p-values
    if( nTailType == 0 )
    {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = TRUE )
    }
    else
    {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = FALSE )
    }

    # Create the graph object and apply the gMCP procedure
    cGraph  <- gMCPLite::matrix2graph( m = mTransition, weights = vWeights )
    cOutput <- gMCPLite::gMCP( graph = cGraph, pvalues = vRawPValues,
                               est = "Bonferroni", alpha = dAlpha )

    lRet <- list( raw.p.values = vRawPValues,
                  adj.p.values = cOutput@adjPValues,
                  decisionFlag = as.numeric( cOutput@rejected ) )
    return( lRet )
}
