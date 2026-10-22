######################################################################################################################## .
#' @name AnalyzeSubpopulation
#' @title Analyze time-to-event subject outcomes
#' @description Computes test statistic, hazard ratio, analysis time and decision at a given interim analysis while
#'   having multiple subpopulations.
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
#'         multiple testing procedure via \code{ComputeGMCPDecisions()}.
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
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#' }
#'
#' The subject-level factor columns referenced by DesignParam$SubPops must also be present in SimData,
#'   with each row containing the corresponding subject's factor levels.
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{NumSubPops}{Integer. Number of subpopulations. East Horizon Explore: Only available for `Endpoint Type =
#'     Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design: Not available.}
#'   \item{SubPops}{Named List. Named list of length equal to the number of subpopulations, indicating factor
#'     details. For example, `SubPops["Factor1"]` is an array of level names for Factor 1. East Horizon Explore:
#'     Only available for `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on.
#'     East Horizon Design: Not available.}
#'   \item{SubpopName}{Array of Character. Array of length equal to the number of subpopulations, indicating the
#'     subpopulation names. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event` with
#'     `Stratification` and `Subpopulations` turned on. East Horizon Design: Not available.}
#'   \item{TransitionMatrix}{Matrix of Numeric. Transition matrix between all populations, including full
#'     population and subpopulations. Dimension of `(NumSubPops + 1) x (NumSubPops + 1)`. East Horizon Explore:
#'     Only available for `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on.
#'     East Horizon Design: Not available.}
#'   \item{PropAlpha}{Vector of Numeric. Vector of length `(NumSubPops + 1)`, indicating the proportion of alpha
#'     for all populations, including full population and subpopulations. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design:
#'     Not available.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{CumEvents}{Integer cumulative number of events at the current look. These examples also accept a vector of
#'     per-look cumulative event counts, indexed by LookInfo$CurrLookIndex. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` and for Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test =
#'     1 - Ratio of Proportions or 1 - Ratio of Poisson Rates`). East Horizon Design: Only available for `Time-to-
#'     Event` tests.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Named list of integer boundary-crossing codes, with one element for the full
#'     population and each subpopulation, using the population names defined by the design: 0 = no
#'     boundary crossed; 1 = lower efficacy boundary crossed; 2 = upper efficacy boundary crossed; 3 =
#'     futility boundary crossed; 4 = equivalence boundary crossed (unavailable in East Horizon
#'     Explore).}
#'   \item{TestStat}{Named list of numeric test statistics on the Wald (Z) scale, with one element for
#'     the full population and each subpopulation, using the population names defined by the design.}
#'   \item{HR}{Named list of estimated treatment-to-control hazard ratios, with one element for the full
#'     population and each subpopulation, using the population names defined by the design.}
#'   \item{dHR}{Custom alias of HR retained for scripts that consumed the original example output.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' SimData also contains the subject-level stratification-factor columns named in DesignParam$StratFactors. When
#'   subpopulations are enabled, Decision, TestStat, and HR are named lists with one element for the full
#'   population and each subpopulation, using the population names defined by the design.
######################################################################################################################## .

AnalyzeSubpopulation <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    dTimeOfAnalysis <- 0

    # Step 1: Determine number of events for analysis
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$CumEvents
        if ( length( nQtyOfEvents ) > 1 ) {
            nQtyOfEvents <- nQtyOfEvents[ nLookIndex ]
        }
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Step 2: Prepare analysis dataset
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis,
        dTimeOfAnalysis - SimData$ArrivalTime,
        SimData$TimeOfEvent - SimData$ArrivalTime
    )

    # Step 3: Read population inputs
    nNumSubPops <- DesignParam$NumSubPops
    vPopNames <- DesignParam$SubpopName
    lSubPops <- DesignParam$SubPops
    vPropAlpha <- DesignParam$PropAlpha

    # Step 4: Create population filters
    lPopFilters <- list( )
    lPopFilters[[ "Full Population" ]] <- rep( TRUE, nrow( SimData ) )

    if ( nNumSubPops > 0 ) {
        for ( strSubpopName in names( lSubPops ) ) {
            vSubpopFilter <- rep( TRUE, nrow( SimData ) )
            for ( strFactorName in names( lSubPops[[ strSubpopName ]] ) ) {
                vAllowedValues <- lSubPops[[ strSubpopName ]][[ strFactorName ]]
                vSubpopFilter <- vSubpopFilter & ( SimData[[ strFactorName ]] %in% vAllowedValues )
            }
            lPopFilters[[ strSubpopName ]] <- vSubpopFilter
        }
    }

    # Step 5: Determine all possible factors across all subpopulations
    vAllFactors <- unique( unlist( lapply( lSubPops, names ) ) )
    for ( strFactor in vAllFactors ) {
        if ( strFactor %in% names( SimData ) ) {
            SimData[[ strFactor ]] <- factor( SimData[[ strFactor ]],
                levels = unique( SimData[[ strFactor ]] )
            )
        }
    }

    # Step 6: Initialize output lists
    lTestStatistics <- list( )
    lHazardRatios <- list( )
    lDecisions <- list( )
    vTestStats <- c( )
    vPopOrder <- c( )

    # Step 7: Compute test statistics and collect populations
    for ( strPopName in names( lPopFilters ) ) {
        dfSubsetData <- SimData[ lPopFilters[[ strPopName ]], ]

        if ( nrow( dfSubsetData ) > 0 ) {
            # Identify stratification factors
            if ( strPopName == "Full Population" ) {
                vCurrentStratFactors <- vAllFactors
            } else {
                vCurrentStratFactors <- names( lSubPops[[ strPopName ]] )
            }
            vCurrentStratFactors <- vCurrentStratFactors[ vCurrentStratFactors %in% names( dfSubsetData ) ]

            # Build survival formula
            if ( length( vCurrentStratFactors ) > 0 ) {
                fStrataFormula <- stats::as.formula(
                    paste0(
                        "survival::Surv(ObservedTime, Event) ~ TreatmentID + ",
                        paste0( "survival::strata(`", vCurrentStratFactors, "`)", collapse = " + " )
                    )
                )
            } else {
                fStrataFormula <- survival::Surv( ObservedTime, Event ) ~ TreatmentID
            }

            # Estimate the hazard ratio
            cCoxFit <- survival::coxph( fStrataFormula, data = dfSubsetData )
            dHazardRatio <- exp( stats::coef( cCoxFit ) )

            # Run the log-rank test
            cSurvDiff <- survival::survdiff( fStrataFormula, data = dfSubsetData )
            dTestStat <- sqrt( cSurvDiff$chisq )
            dTestStat <- ifelse( unname( dHazardRatio ) < 1, dTestStat * -1, dTestStat )

            # Store outputs in named lists
            lTestStatistics[[ strPopName ]] <- as.double( dTestStat )
            lHazardRatios[[ strPopName ]] <- as.double( dHazardRatio )
            vTestStats <- c( vTestStats, dTestStat )
            vPopOrder <- c( vPopOrder, strPopName )
        } else {
            lTestStatistics[[ strPopName ]] <- NA
            lHazardRatios[[ strPopName ]] <- NA
            vTestStats <- c( vTestStats, NA )
            vPopOrder <- c( vPopOrder, strPopName )
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
    for ( iPop in seq_along( vPopOrder ) ) {
        strPopName <- vPopOrder[ iPop ]
        nGMCPFlag <- lGMCPResult$decisionFlag[ iPop ]

        if ( nGMCPFlag == 1 ) {
            nFinalDecision <- ifelse( DesignParam$TailType == 0, 1L, 2L )
        } else if ( nLookIndex == nQtyOfLooks ) {
            nFinalDecision <- 3
        } else {
            nFinalDecision <- 0
        }

        lDecisions[[ strPopName ]] <- as.integer( nFinalDecision )
    }

    # Step 10: Return results
    lRet <- list(
        Decision = as.list( lDecisions ),
        TestStat = as.list( lTestStatistics ),
        HR = as.list( lHazardRatios ),
        dHR = as.list( lHazardRatios ),
        AnalysisTime = as.double( dTimeOfAnalysis ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lRet )
}

ComputeGMCPDecisions <- function( vTestStats, nTailType, dAlpha, vWeights, mTransition ) {
    bTestStatMissing <- is.nan( vTestStats ) | is.na( vTestStats )
    if ( any( bTestStatMissing ) ) {
        vTestStats[ which( bTestStatMissing == TRUE ) ] <- ifelse( nTailType == 0, Inf, -Inf )
    }

    # Compute raw p-values
    if ( nTailType == 0 ) {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = TRUE )
    } else {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = FALSE )
    }

    # Create the graph object and apply the gMCP procedure
    cGraph <- gMCPLite::matrix2graph( m = mTransition, weights = vWeights )
    cOutput <- gMCPLite::gMCP(
        graph = cGraph, pvalues = vRawPValues,
        test = "Bonferroni", alpha = dAlpha
    )

    lRet <- list(
        raw.p.values = vRawPValues,
        adj.p.values = cOutput@adjPValues,
        decisionFlag = as.numeric( cOutput@rejected )
    )
    return( lRet )
}
