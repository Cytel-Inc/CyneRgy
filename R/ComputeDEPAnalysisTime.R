######################################################################################################################## .
#' @name ComputeDEPAnalysisTime
#' @title Compute Analysis Time for Dual Endpoint (DEP) Designs
#'
#' @description Computes the calendar analysis time for trials with one or two endpoints, supporting both
#' Fixed Sample Designs and Group Sequential Designs. The function determines when the planned
#' number of events or completers has been reached based on simulated trial data and design parameters.
#'
#' For Group Sequential Designs, the analysis time is determined according to the current interim look and
#' synchronization endpoint. For Fixed Sample Designs, the analysis time corresponds to the final look when
#' all required events or completers are observed.
#'
#' @param SimData A data frame containing the simulated subject-level data with the following required columns:
#' \describe{
#'   \item{ClndrRespTime}{Response times for Endpoint 1.}
#'   \item{CensorIndOrg}{Censoring indicators for Endpoint 1.}
#'   \item{ClndrRespTime2}{Response times for Endpoint 2.}
#'   \item{CensorIndOrg2}{Censoring indicators for Endpoint 2.}
#' }
#' @param DesignParam A list of design parameters containing:
#' \describe{
#'   \item{EndpointType}{Numeric vector specifying endpoint types (1 = completer, 2 = event).}
#'   \item{EndpointName}{Character vector of endpoint names.}
#'   \item{PlanEndTrial}{Integer specifying which endpoint(s) define the trial end:
#'                      \describe{
#'                        \item{1}{Both endpoints.}
#'                        \item{2}{Endpoint 1 only.}
#'                        \item{3}{Endpoint 2 only.}
#'                      }}
#'   \item{MaxCompleters}{List of target numbers of completers for each endpoint.}
#'   \item{MaxEvents}{List of target numbers of events for each endpoint.}
#'   \item{SampleSize}{Total sample size.}
#' }
#' @param LookInfo Optional list specifying Group Sequential Design information (default = \code{NULL}).
#' When provided, indicates that a GSD is used and must include:
#' \describe{
#'   \item{SyncInterim}{Endpoint ID used for interim look positioning.}
#'   \item{NumLooks}{Total number of looks planned.}
#'   \item{CurrLookIndex}{Current look index.}
#'   \item{NumEndpointLooks}{Numeric vector giving the number of looks per endpoint.}
#'   \item{CumEvents}{List of cumulative event targets per endpoint.}
#'   \item{CumCompleters}{List of cumulative completer targets per endpoint.}
#' }
#' @return A numeric value representing the calendar analysis time (in the same units as \code{ClndrRespTime}).
#' @export
######################################################################################################################## .


ComputeDEPAnalysisTime <- function(SimData, DesignParam, LookInfo = NULL) 
{
    
    bGSD <- ifelse(is.null(LookInfo), FALSE, TRUE)                                # Is the trial using Group sequential Design?
    
    if(bGSD)               # Group Sequential Design   
    {
        syncEPID <- LookInfo$SyncInterim                                            # Endpoint ID for the endpoint to be used for look positioning
        syncEPType <- DesignParam$EndpointType[[syncEPID]]                          # Endpoint type of the endpoint used for look positioning
        nonsyncEPID <- ifelse(LookInfo$SyncInterim == 1, 2, 1)                      # Endpoint ID for endpoint not being used for look positioning
        nonsyncEPType <- DesignParam$EndpointType[[nonsyncEPID]]                    # Endpoint type of the endpoint not being used for look positioning
        
        # Look info was provided so use it
        nQtyOfLooks         <- LookInfo$NumLooks
        nLookIndex          <- LookInfo$CurrLookIndex
        
        # CumTargets will be planned cumulative events/completers for the Endpoint used for the current look positioning.
        if(nLookIndex <= LookInfo$NumEndpointLooks[syncEPID]) 
        {
            if(syncEPType == 2 )
            {
                CumTargets    <- LookInfo$CumEvents[[DesignParam$EndpointName[syncEPID]]]
            }
            else
            {
                CumTargets    <- LookInfo$CumCompleters[[DesignParam$EndpointName[syncEPID]]]
            }
        } 
        else
        {
            if(nonsyncEPType == 2 )
            {
                CumTargets    <- LookInfo$CumEvents[[DesignParam$EndpointName[nonsyncEPID]]]
            }
            else
            {
                CumTargets    <- LookInfo$CumCompleters[[DesignParam$EndpointName[nonsyncEPID]]]
            }
        }
        
        nQtyOfTargets <- CumTargets[nLookIndex]
        
        EPIDforSlicingData <- ifelse(nLookIndex <= LookInfo$NumEndpointLooks[syncEPID] , syncEPID, nonsyncEPID) 
        if(EPIDforSlicingData == 1) 
        {
            SimDataAnlys <- SimData[order(SimData$ClndrRespTime, SimData$CensorIndOrg), ]
            idxAnlys <- which(cumsum(SimDataAnlys$CensorIndOrg) >= nQtyOfTargets)
            AnalysisTime <- ifelse(length(idxAnlys) > 0, 
                                   SimDataAnlys$ClndrRespTime[min(idxAnlys)], 
                                   SimDataAnlys$ClndrRespTime[DesignParam$SampleSize])
        }
        else 
        {
            SimDataAnlys <- SimData[order(SimData$ClndrRespTime2, SimData$CensorIndOrg2), ]
            idxAnlys <- which(cumsum(SimDataAnlys$CensorIndOrg2) >= nQtyOfTargets)
            AnalysisTime <- ifelse(length(idxAnlys) > 0, 
                                   SimDataAnlys$ClndrRespTime2[min(idxAnlys)], 
                                   SimDataAnlys$ClndrRespTime2[DesignParam$SampleSize]) 
        }
    }
    
    else                        #FSD design
    {   
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        
        
        # nQtyOfTargets will be planned events/completers for the Endpoint on which end of the trial is defined.
        if(DesignParam$PlanEndTrial == 2 || DesignParam$PlanEndTrial == 1)  #Full info on Endpoint 1 or Both Endpoints
        {
            nQtyOfTargets <- ifelse( DesignParam$EndpointType[1] == 1,  
                                     DesignParam$MaxCompleters[[DesignParam$EndpointName[1]]],
                                     DesignParam$MaxEvents[[DesignParam$EndpointName[1]]] )
            SimDataEP1 <- SimData[order(SimData$ClndrRespTime, SimData$CensorIndOrg), ]
            idxEP1 <- which(cumsum(SimDataEP1$CensorIndOrg) >= nQtyOfTargets)
            AnalysisTimeEP1 <- ifelse(length(idxEP1) > 0, 
                                      SimDataEP1$ClndrRespTime[min(idxEP1)], 
                                      SimDataEP1$ClndrRespTime[DesignParam$SampleSize])
            
        }
        if(DesignParam$PlanEndTrial == 3 || DesignParam$PlanEndTrial == 1)  #Full info on Endpoint 2 or Both Endpoints
        {
            nQtyOfTargets <- ifelse( DesignParam$EndpointType[2] == 1,  
                                     DesignParam$MaxCompleters[[DesignParam$EndpointName[2]]],
                                     DesignParam$MaxEvents[[DesignParam$EndpointName[2]]] )
            SimDataEP2 <- SimData[order(SimData$ClndrRespTime2, SimData$CensorIndOrg2), ]
            idxEP2 <- which(cumsum(SimDataEP2$CensorIndOrg2) >= nQtyOfTargets)
            AnalysisTimeEP2 <- ifelse(length(idxEP2) > 0, 
                                      SimDataEP2$ClndrRespTime2[min(idxEP2)], 
                                      SimDataEP2$ClndrRespTime2[DesignParam$SampleSize])
            
        }    
        
        AnalysisTime <- ifelse( DesignParam$PlanEndTrial == 1, max(AnalysisTimeEP1, AnalysisTimeEP2), 
                                ifelse(DesignParam$PlanEndTrial == 2, AnalysisTimeEP1, AnalysisTimeEP2) )
    }
    return(AnalysisTime)
}