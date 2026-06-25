########################################################################################################################
#' @name AnalyzeUsingLogrankTestBonferroni
#' @title Analyze multi-arm time-to-event outcomes using Bonferroni-adjusted log-rank tests.
#'
#' @description
#' Performs treatment-versus-control comparisons for multi-arm survival trials using
#' the log-rank test with Bonferroni adjustment for multiplicity. Hazard ratios are
#' estimated using Cox proportional hazards models.
#'
#' The function supports both fixed-sample and group sequential designs. For
#' group sequential designs, patients are administratively censored at the current
#' interim analysis based on the planned number of events. Treatment decisions are
#' made by comparing Bonferroni-adjusted p-values with the efficacy boundary
#' corresponding to the current analysis.
#'
#' @param SimData Data frame containing one row per simulated patient. The analysis
#'   uses the following variables:
#'   \describe{
#'     \item{ArrivalTime}{Patient enrollment time.}
#'     \item{TreatmentID}{Treatment assignment (control arm = 0).}
#'     \item{SurvivalTime}{Event or censoring time measured from enrollment.}
#'     \item{DropOutTime}{Optional dropout time (not used by this function).}
#'   }
#'
#' @param DesignParam List containing trial design parameters. Commonly used fields
#'   include:
#'   \describe{
#'     \item{Alpha}{One-sided Type I error rate.}
#'     \item{TailType}{Tail direction (0 = left, 1 = right).}
#'     \item{NumTreatments}{Number of experimental treatment arms.}
#'     \item{MaxEvents}{Target number of events at the final analysis.}
#'     \item{CriticalPoint}{Critical value for fixed-sample designs.}
#'     \item{IsArmPresent}{Indicator vector specifying which treatment arms remain active.}
#'   }
#'
#' @param LookInfo Optional list containing interim analysis information for group
#'   sequential designs. Commonly used fields include:
#'   \describe{
#'     \item{NumLooks}{Total number of analyses.}
#'     \item{CurrLookIndex}{Current analysis index.}
#'     \item{InfoFrac}{Information fraction at each analysis.}
#'     \item{EffBdry}{Efficacy boundaries on the Z scale.}
#'   }
#'
#' @param OutList Optional list used to pass interim outputs between analyses.
#'
#' @param UserParam Optional user-defined list of additional parameters.
#'
#' @return A list containing:
#' \describe{
#'   \item{Decision}{Integer decision code for each treatment arm:
#'     \describe{
#'       \item{NA}{Treatment arm was previously dropped.}
#'       \item{0}{Continue to the next analysis.}
#'       \item{2}{Efficacy boundary crossed.}
#'       \item{3}{Futility at the final analysis.}
#'     }}
#'   \item{HR}{Estimated hazard ratio for each treatment arm versus control.}
#'   \item{HazardRatio}{Alias of \code{HR} for backward compatibility.}
#'   \item{RawPVal}{Unadjusted log-rank p-values.}
#'   \item{AdjPVal}{Bonferroni-adjusted p-values.}
#'   \item{AnalysisTime}{Calendar time at which the analysis was performed.}
#'   \item{ErrorCode}{Error indicator (0 = success).}
#' }
########################################################################################################################

AnalyzeUsingLogrankTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, OutList = NULL, UserParam = NULL )
{
    library( "survival" )
    
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks     <- LookInfo$NumLooks
        nLookIndex      <- LookInfo$CurrLookIndex
        nQtyOfEvents    <- LookInfo$InfoFrac[ nLookIndex ] * DesignParam$MaxEvents
        dEffBoundary    <- LookInfo$EffBdry[ nLookIndex ]
        
        dBoundaryPScale <- if( DesignParam$TailType == 1 )
        {
            1 - pnorm( dEffBoundary )
        }
        else
        {
            pnorm( dEffBoundary )
        }
    }
    else
    {
        nQtyOfLooks     <- 1
        nLookIndex      <- 1
        nQtyOfEvents    <- DesignParam$MaxEvents
        dBoundaryPScale <- DesignParam$Alpha
    }
    
    vIsTrtPresent         <- DesignParam$IsArmPresent
    dfSimData             <- SimData
    
    dfSimData$TimeOfEvent <- dfSimData$ArrivalTime + dfSimData$SurvivalTime
    
    dfSimData             <- dfSimData[ order( dfSimData$TimeOfEvent ), ]
    
    if( nrow( dfSimData ) < nQtyOfEvents )
    {
        return( list(
            Decision     = rep( NA_integer_, DesignParam$NumTreatments ),
            ErrorCode    = as.integer( 1 ),
            HR           = rep( NA_real_, DesignParam$NumTreatments ),
            HazardRatio  = rep( NA_real_, DesignParam$NumTreatments ),
            RawPVal      = rep( NA_real_, DesignParam$NumTreatments ),
            AdjPVal      = rep( NA_real_, DesignParam$NumTreatments ),
            AnalysisTime = NA_real_
        ) )
    }
    
    dTimeOfAnalysis             <- dfSimData[ nQtyOfEvents, ]$TimeOfEvent
    
    dfSimData                   <- dfSimData[ dfSimData$ArrivalTime <= dTimeOfAnalysis, ]
    dfSimData$Event             <- ifelse( dfSimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    dfSimData$ObservedTime      <- ifelse(
        dfSimData$TimeOfEvent > dTimeOfAnalysis,
        dTimeOfAnalysis - dfSimData$ArrivalTime,
        dfSimData$TimeOfEvent - dfSimData$ArrivalTime
    )
    
    dfSimData <- dfSimData[ order( dfSimData$ObservedTime ), ]
    
    vPValues  <- rep( NA_real_, DesignParam$NumTreatments )
    vHRRatio  <- rep( NA_real_, DesignParam$NumTreatments )
    
    for( nTrtID in seq_len( DesignParam$NumTreatments ) )
    {
        if( vIsTrtPresent[ nTrtID ] == 1 )
        {
            dfSimDataTrt <- dfSimData[ dfSimData$TreatmentID %in% c( 0, nTrtID ), ]
            
            coxModel <- survival::coxph(
                survival::Surv( ObservedTime, Event ) ~ TreatmentID,
                data = dfSimDataTrt
            )
            
            logrankTest <- survival::survdiff(
                survival::Surv( ObservedTime, Event ) ~ TreatmentID,
                data = dfSimDataTrt
            )
            
            vHRRatio[ nTrtID ] <- as.numeric( exp( coxModel$coefficients ) )
            vPValues[ nTrtID ] <- logrankTest$pvalue
        }
    }
    
    nActiveArms <- sum( vIsTrtPresent == 1, na.rm = TRUE )
    vAdjPValues <- pmin( vPValues * nActiveArms, 1 )
    
    vDecision   <- rep( NA_integer_, DesignParam$NumTreatments )
    
    for( i in seq_len( DesignParam$NumTreatments ) )
    {
        if( vIsTrtPresent[ i ] == 1 )
        {
            if( !is.na( vAdjPValues[ i ] ) && vAdjPValues[ i ] < dBoundaryPScale )
            {
                vDecision[ i ] <- 2L
            }
            else if( nLookIndex == nQtyOfLooks )
            {
                vDecision[ i ] <- 3L
            }
            else
            {
                vDecision[ i ] <- 0L
            }
        }
    }
    
    return( list(
        Decision     = as.integer( vDecision ),
        ErrorCode    = as.integer( 0 ),
        HR           = as.double( vHRRatio ),
        HazardRatio  = as.double( vHRRatio ),
        RawPVal      = as.double( vPValues ),
        AdjPVal      = as.double( vAdjPValues ),
        AnalysisTime = as.double( dTimeOfAnalysis )
    ) )
}
