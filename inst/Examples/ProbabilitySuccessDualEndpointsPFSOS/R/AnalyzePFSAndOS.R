######################################################################################################################## .
#  Last Modified Date: 11/20/2024
#' @name AnalyzeDualEndpointTTE
#' @title Analyze Progression-Free Survival (PFS) and Overall Survival (OS) Data Using Probability of Success (PoS)
#' 
#' @param SimData A data frame containing subject-level data generated during the simulation. Each row corresponds to a patient, 
#'                and the columns include relevant variables such as arrival time, treatment assignment, survival time, and dropout time. Key columns include:
#'                \describe{
#'                  \item{ArrivalTime}{Numeric value representing the time the patient entered the trial.}
#'                  \item{TreatmentID}{Integer value where 0 indicates control treatment and 1 experimental treatment.}
#'                  \item{SurvivalTime}{Numeric value for the survival time or time-to-event for the patient.}
#'                  \item{DropOutTime}{Numeric value for the dropout time for the patient in a time-to-event trial.}
#'                  \item{OS}{Numeric value for overall survival time for the patient.}
#'                }
#' @param DesignParam A list containing design and simulation parameters required to compute test statistics and perform testing. Key elements include:
#'                    \describe{
#'                      \item{SampleSize}{Total sample size of the trial.}
#'                      \item{Alpha}{Type I error rate (significance level).}
#'                      \item{TestType}{Type of test:
#'                                      \describe{
#'                                        \item{0}{One-sided test.}
#'                                        \item{1}{Two-sided symmetric test.}
#'                                        \item{2}{Two-sided asymmetric test.}
#'                                      }}
#'                      \item{TailType}{Tail direction:
#'                                      \describe{
#'                                        \item{0}{Left-tailed test.}
#'                                        \item{1}{Right-tailed test.}
#'                                      }}
#'                      \item{LowerAlpha}{Significance level for lower efficacy boundary in asymmetric tests.}
#'                      \item{UpperAlpha}{Significance level for upper efficacy boundary in asymmetric tests.}
#'                      \item{MaxEvents}{Maximum number of events allowed in the trial.}
#'                      \item{FollowUpType}{Follow-up type for survival tests:
#'                                           \describe{
#'                                             \item{0}{Follow-up until end of study.}
#'                                             \item{1}{Follow-up for a fixed period.}
#'                                           }}
#'                    }
#' @param LookInfo A list containing input parameters related to multiple looks in group sequential designs. Key elements include:
#'                 \describe{
#'                   \item{NumLooks}{Integer value indicating the total number of looks in the study.}
#'                   \item{CurrLookIndex}{Integer value for the current look index, starting from 1.}
#'                   \item{InfoFrac}{Information fraction at the current look.}
#'                   \item{CumAlpha}{Cumulative alpha spent at the current look (one-sided tests).}
#'                   \item{EffBdryScale}{Scale for efficacy boundaries:
#'                                       \describe{
#'                                         \item{0}{Z-scale.}
#'                                         \item{1}{p-value scale.}
#'                                       }}
#'                   \item{EffBdry}{Vector of efficacy boundaries for one-sided tests.}
#'                   \item{EffBdryUpper}{Vector of upper efficacy boundaries for two-sided tests.}
#'                   \item{EffBdryLower}{Vector of lower efficacy boundaries for two-sided tests.}
#'                   \item{FutBdryScale}{Scale for futility boundaries:
#'                                       \describe{
#'                                         \item{0}{Z-scale.}
#'                                         \item{1}{p-value scale.}
#'                                         \item{2}{Delta scale.}
#'                                         \item{3}{Conditional power scale.}
#'                                       }}
#'                   \item{FutBdry}{Vector of futility boundaries for one-sided tests.}
#'                   \item{FutBdryUpper}{Vector of upper futility boundaries for two-sided tests.}
#'                   \item{FutBdryLower}{Vector of lower futility boundaries for two-sided tests.}
#'                 }
#' @param UserParam A list of user-defined parameters. These custom scalar variables can be of types Integer, Numeric, or Character. Relevant elements include:
#'                  \describe{
#'                    \item{HazardRatioCutoffIA}{OS hazard ratio threshold for interim analysis.}
#'                    \item{HazardRatioCutoffFA}{OS hazard ratio threshold for final analysis.}
#'                  }
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
#'           \item{TestStat}{Numeric test statistic value (required if Decision is not returned).}
#'           \item{ErrorCode}{Optional integer value:
#'                            \describe{
#'                              \item{0}{No error.}
#'                              \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                              \item{< 0}{Fatal error; no further simulations are attempted.}
#'                            }}
#'           \item{Delta}{Hazard ratio (optional numeric value). Used in Solara for creating the observed hazard ratio graph. Applicable for time-to-event data.}
#'           \item{nPFSEfficacy}{Indicator for PFS efficacy decision:
#'                              \describe{
#'                                \item{0}{No PFS efficacy decision.}
#'                                \item{1}{PFS efficacy decision made.}
#'                              }}
#'           \item{nOSEfficacy}{Indicator for OS efficacy decision:
#'                              \describe{
#'                                \item{0}{No OS efficacy decision.}
#'                                \item{1}{OS efficacy decision made.}
#'                              }}
#'           \item{dPValuePFS}{P-value for progression-free survival endpoint.}
#'           \item{dZValPFS}{Z-value for progression-free survival endpoint.}
#'           \item{dPValueOS}{P-value for overall survival endpoint.}
#'           \item{dHazardRatioPFS}{Hazard ratio for progression-free survival endpoint.}
#'           \item{dHazardRatioOS}{Hazard ratio for overall survival endpoint.}
#'           \item{dEffBdry}{Efficacy boundary value for the current look.}
#'           \item{HazardRatioCutoffIA}{Hazard ratio threshold for interim analysis.}
#'           \item{HazardRatioCutoffFA}{Hazard ratio threshold for final analysis.}
#'         }
######################################################################################################################## .

AnalyzePFSAndOS <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    library( survival )
    library( CyneRgy )
    
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks  <- LookInfo$NumLooks
        nLookIndex   <- LookInfo$CurrLookIndex
        CumEvents    <- LookInfo$InfoFrac*DesignParam$MaxEvents
        nQtyOfEvents <- CumEvents[ nLookIndex ]
        dEffBdry     <- LookInfo$EffBdryLower[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        nQtyOfEvents <- DesignParam$MaxEvents 
        dEffBdry     <- DesignParam$CriticalPoint
    }
    
    # Build the dataset 
    SimData$TimeOfPFSEvent    <- SimData$ArrivalTime + SimData$SurvivalTime    
    SimData$TimeOfOSEvent     <- SimData$ArrivalTime + SimData$OS 
    SimData                   <- SimData[ order( SimData$TimeOfPFSEvent), ]
    dTimeOfAnalysis           <- SimData[ nQtyOfEvents, ]$TimeOfPFSEvent
    SimData                   <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]   
    
    # Set the PFS event and observed times
    SimData$Event             <- ifelse( SimData$TimeOfPFSEvent > dTimeOfAnalysis, 0, 1 )  
    SimData$ObservedTime      <- ifelse( SimData$TimeOfPFSEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfPFSEvent - SimData$ArrivalTime )
    
    # Set the OS event and observed times
    SimData$OSEvent             <- ifelse( SimData$TimeOfOSEvent > dTimeOfAnalysis, 0, 1 )  
    SimData$ObservedTimeOS      <- ifelse( SimData$TimeOfOSEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfOSEvent - SimData$ArrivalTime )
    
    # Analyze the PFS data
    fitCox          <- coxph( Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )
    dPValuePFS      <- summary( fitCox )$coefficients[ ,"Pr(>|z|)" ]
    dZValPFS        <- summary( fitCox )$coefficients[ ,"z" ]
    dHazardRatioPFS <- exp( coef( fitCox ) )
    
    # Analyze the OS data
    fitCoxOS        <- coxph( Surv( ObservedTimeOS, Event ) ~ as.factor( TreatmentID ), data = SimData )
    dPValueOS       <- summary( fitCoxOS )$coefficients[ ,"Pr(>|z|)" ]
    dHazardRatioOS  <- exp(coef( fitCoxOS ) )
    dZValOS         <- summary( fitCoxOS )$coefficients[ ,"z" ]
    
    nPFSEfficacy    <- 0   # 0 if NOT an efficacy decision for PFS, 1 if efficacy decision for PFS
    nOSEfficacy     <- 0   # 0 if NOT an efficacy decision for OS, 1 if efficacy decision for OS
    if( nLookIndex < nQtyOfLooks )  # Interim Analysis
    {
        if( dZValPFS <= dEffBdry  )
        {
            nPFSEfficacy <- 1
        }
        if( dHazardRatioOS < UserParam$HazardRatioCutoffIA )
        {
            nOSEfficacy  <- 1
        }
        
        if( dZValPFS <= dEffBdry && dHazardRatioOS < UserParam$HazardRatioCutoffIA )
        {
            strDecision <- "Efficacy"
            
        }
        else
        {
            strDecision <- "Continue"
        }
    }
    else # Final Analysis
    {
        
        if( dZValPFS <= dEffBdry  )
        {
            nPFSEfficacy <- 1
        }
        if( dHazardRatioOS < UserParam$HazardRatioCutoffFA )
        {
            nOSEfficacy  <- 1
        }
        
        
        if(  dZValPFS <= dEffBdry && dHazardRatioOS < UserParam$HazardRatioCutoffFA )
        {
            strDecision <- "Efficacy"
        }
        else
        {
            strDecision <- "Futility"
        }
    }
    
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    lRet <- list( TestStat            = as.double( dZValPFS ),
                  Decision            = as.integer( nDecision ), 
                  nPFSEfficacy        = as.integer( nPFSEfficacy ),
                  nOSEfficacy         = as.integer( nOSEfficacy ),
                  dPValuePFS          = as.double( dPValuePFS ),
                  dZValPFS            = as.double( dZValPFS), 
                  dPValueOS           = as.double( dPValueOS ),
                  dHazardRatioPFS     = as.double( dHazardRatioPFS ), 
                  dHazardRatioOS      = as.double( dHazardRatioOS ),
                  dEffBdry            = as.double( dEffBdry ), 
                  HazardRatioCutoffIA = as.double( UserParam$HazardRatioCutoffIA ),
                  HazardRatioCutoffFA = as.double( UserParam$HazardRatioCutoffFA ),
                  ErrorCode           = as.integer( 0 ),
                  HazardRatio         = as.double( dHazardRatioPFS ) )
    
    return( lRet )
}
