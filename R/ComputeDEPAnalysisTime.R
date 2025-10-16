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