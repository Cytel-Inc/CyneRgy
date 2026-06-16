
########################################################################################################################
#' @name AnalyzeUsingLogrankTestBonferroni
#' @title Analyze time-to-event outcomes for multi-arm designs using log-rank tests with multiple comparison adjustment.
#' @description
#' Collection of analysis functions for multi-arm multi-stage (MAMS) survival designs.
#' These functions perform treatment-versus-control comparisons using the log-rank
#' test and Cox proportional hazards models. The file supports fixed sample and
#' group sequential designs for multiple comparison procedure Bonferroni.
#'
#' @param SimData Data frame with one row per patient generated during simulation.
#'        Important variables used by the analysis functions include:
#'        \describe{
#'          \item{ArrivalTime}{Numeric value indicating patient enrollment time.}
#'          \item{TreatmentID}{Integer treatment assignment where control arm is indexed as 0.}
#'          \item{SurvivalTime}{Numeric event or censoring time generated for each patient.}
#'          \item{SimulationID}{Optional simulation index used for saving debug inputs.}
#'        }
#'
#' @param DesignParam List containing trial design parameters.
#'        Commonly used fields include:
#'        \describe{
#'          \item{Alpha}{One-sided Type I error rate.}
#'          \item{TailType}{Tail direction. Right tailed: 1, Left tailed: 0.}
#'          \item{NumTreatments}{Number of experimental treatment arms.}
#'          \item{MaxEvents}{Maximum number of events for the study.}
#'          \item{CriticalPoint}{Critical value for fixed sample designs.}
#'          \item{MultAdjMethod}{Multiple comparison procedure identifier.}
#'          \item{AlphaProp}{Alpha allocation weights for weighted procedures.}
#'          \item{TestSeq}{Testing sequence for fixed-sequence and fallback procedures.}
#'          \item{IsArmPresent}{Vector indicating whether treatment arms remain active at the current analysis.}
#'        }
#'
#' @param LookInfo Optional list containing interim analysis information.
#'        Common fields include:
#'        \describe{
#'          \item{NumLooks}{Total number of analyses.}
#'          \item{CurrLookIndex}{Current analysis index.}
#'          \item{InfoFrac}{Information fraction at each analysis.}
#'          \item{EffBdry}{Efficacy boundary values.}
#'          \item{CumEvents}{Cumulative event counts by look.}
#'        }
#'
#' @param OutList Optional list used to pass interim output information between analyses.
#' @param UserParam Optional user-defined list of custom scalar parameters.
#'
#' @return Each function returns a list containing one or more of the following:
#'         \describe{
#'           \item{Decision}{Integer decision code for each treatment arm.}
#'           \item{HR / HazardRatio}{Observed hazard ratio from Cox proportional hazards model.}
#'           \item{AnalysisTime}{Calendar time of the current analysis.}
#'           \item{ErrorCode}{Error indicator. 0 indicates success.}
#'         }
########################################################################################################################

AnalyzeUsingLogrankTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, OutList = NULL, UserParam = NULL )
{
    require( survival )
    # Retrieve necessary information from the objects East Horizon sent
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nQtyOfLooks              <- LookInfo$NumLooks
        nLookIndex               <- LookInfo$CurrLookIndex
        dCumEvents               <- LookInfo$InfoFrac*DesignParam$MaxEvents
        nQtyOfEvents             <- dCumEvents[ nLookIndex ]
        vInfoFrac                <- LookInfo$InfoFrac
        vEfficacyBoundary        <- LookInfo$EffBdry[ nLookIndex ]


        if( DesignParam$TailType == 1 ) {
            vEfficacyBoundaryPScale  <- 1 - pnorm( vEfficacyBoundary )
        } else {
            vEfficacyBoundaryPScale  <- pnorm( vEfficacyBoundary )
        }
    }
    else
    {   # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks              <- 1
        nLookIndex               <- 1
        nQtyOfEvents             <- DesignParam$MaxEvents
        dEffBdry                 <- DesignParam$CriticalPoint
        vInfoFrac                <- 1
        vEfficacyBoundaryPScale  <- DesignParam$Alpha
    }

    vIsTrtPresent                <- DesignParam$IsArmPresent
    dfSimData                    <- SimData
    dfSimData$TimeOfEvent        <- dfSimData$ArrivalTime + dfSimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # Compute the time of analysis 
    dfSimData                    <- dfSimData[ order( dfSimData$TimeOfEvent ), ]
    dTimeOfAnalysis              <- dfSimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    dfSimData                    <- dfSimData[ dfSimData$ArrivalTime <= dTimeOfAnalysis , ]   # Exclude any patients that were not enrolled by the time of the analysis
    dfSimData$Event              <- ifelse( dfSimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored
    dfSimData$ObservedTime       <- ifelse( dfSimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - dfSimData$ArrivalTime, dfSimData$TimeOfEvent - dfSimData$ArrivalTime )
    
    # Order the data by observed time for the remainder of the computations
    dfSimData                    <- dfSimData[ order( dfSimData$ObservedTime ), ]
    
    vPValues                     <- rep( NA, DesignParam$NumTreatments )
    vHRRatio                     <- rep( NA, DesignParam$NumTreatments )

    for ( nTrtID in 1:DesignParam$NumTreatments )
    {
        if ( vIsTrtPresent[ nTrtID ] == 1 )
        {
            SimDataTrt           <- dfSimData[ dfSimData$TreatmentID %in% c( 0, nTrtID ), ]
            # Compute Observed HR
            coxModel             <- coxph( Surv( ObservedTime, Event ) ~ TreatmentID, data = SimDataTrt )
            dTrueHR              <- exp( coxModel$coefficients )
            
            # Compute the test statistic using survival package
            logrankTest          <- survdiff( Surv( ObservedTime, Event ) ~ TreatmentID, SimDataTrt )
            
            # Compute the logrank test statistic
            dPValue              <- logrankTest$pvalue
        }
        else
        {
            dTrueHR              <- NA
            dPValue              <- NA
        }
        vHRRatio[ nTrtID ]       <- dTrueHR
        vPValues[ nTrtID ]       <- dPValue
    }
    
    # Calculate Bonferroni adjusted p values
    # Assumes that each present arm has a valid hypothesis test and p-value
    vAdjPValues                  <- vPValues * sum( vIsTrtPresent )
    
    # Perform the desired analysis. NA should be returned for arms that are not available at this look
    vDecision                    <- ifelse( vAdjPValues < vEfficacyBoundaryPScale, 2, 0 )  # A decision of 2 means success, 0 means continue the trial
    
    for( i in 1:length( vDecision ) ){
         if ( !is.na( vDecision[i] ) && vDecision[i] == 0 )
         {            
            # Did not hit efficacy, so check futility 
            # We are at the FA, efficacy decision was not made yet so the decision is futility
            if( nLookIndex == nQtyOfLooks ) 
            {
                vDecision[i]    <- 3 # Code for futility
            }
        }
    }
    
    nError 	                    <- 0
    return( list( Decision      = as.integer( vDecision ),
                  ErrorCode     = as.integer( nError ),
                  HR            = as.double( vHRRatio ),
                  AnalysisTime  = as.double( dTimeOfAnalysis ) ) )
}

    
