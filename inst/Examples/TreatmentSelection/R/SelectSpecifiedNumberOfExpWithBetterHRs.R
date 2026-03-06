######################################################################################################################## .
#' Select user-specified number of treatments to advance that have the better Hazard Ratios. 
#'@param SimData Dataframe which consists of data generated in current simulation
#'@param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#'@param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#'@param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#' \item{UserParam$QtyOfArmsToSelect}{A value that defines how many treatment arms are chosen to advance. 
#'                          Note this number must match the number of user-specified allocation values.
#'                          If this value is not specified, the default is 1.}  
#' \item{UserParam$UpdatedAllocationRatio}{A value that specifies the allocation to the the selected treatment arms
#'                             If this value is not specified, the original allocation ratios will be used.}
#'          }
#'@description
#'This function is used for the MAMS design with a TTE outcome and will perform treatment selection at the interim analysis (IA).   
#'At the IA, the user-specified number of experimental treatments (QtyOfArmsToSelect) that have the better Hazard Ratios are selected.
#'After the IA, we would like to update Allocaiton ratio based on user specified inputs: UpdatedAllocationRatio. If not provided, the initial allocation ratios for the select arms will be used.
#' @return TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments
#' @return AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#'                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#' @examples  Example Output Object:
#'       #Treatment Arms = 4, initial allocation ratios: 1:2:2:2:1 for Control:TrmtArm1:TrmtArm2:TrmtArm3:TrmtArm4 
#'       Example 1: UserParam$UpdatedAllocationRatio' is not specified
#'       Assuming the 1st and 3rd arm is selected,
#'       vSelectedTreatments <- c( 1, 3 )   
#'       vAllocationRatio    <- c( 2, 2 ) # TrmtArm1 and TrmtArm3 both have an allocation ratio of 2.
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'       
#'      Example 2:  UserParam$UpdatedAllocationRatio' is specified as c(2.5, 1.5) 
#'       Assuming the 1st and 3rd arm is selected,
#'       vSelectedTreatments <- c( 1, 3 )  
#'       vAllocationRatio    <- c( 2.5, 1.5 ) # TrmtArm1 and TrmtArm3 have allocation ratios 2.5 and 1.5, respectively.
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export

SelectSpecifiedNumberOfExpWithBetterHRs  <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
 
    saveRDS( SimData,     "SimData.Rds")
    saveRDS( DesignParam, "DesignParam.Rds" )
    saveRDS( LookInfo,    "LookInfo.Rds" )
    
    if( !exists( "UserParam" ) | is.null( UserParam ) )
    {
        # Default is to select the treatment arm with best Hazard Ratio 
        UserParam <- list( QtyOfArmsToSelect = 1)
    }
 
    # Computing the Hazard Ratios of all treatment arms in reference to control arm.
    library(survival)

    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
        TailType             <- DesignParam$TailType
    }
    else
    {   # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfEvents         <- DesignParam$MaxEvents
        TailType             <- DesignParam$TailType
    }
    
    # Step 2: Preparing the data for Hazard Ratio computation ####
    SimData$TimeOfEvent      <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # Compute the time of analysis 
    SimData                  <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis          <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    SimData                  <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event            <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored 
    SimData$ObservedTime     <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    # Order the data by observed time for the remainder of the computations
    SimData                  <- SimData[ order( SimData$ObservedTime), ]
    
    
    # Step 3 : Computing Hazard Ratios ####
    vTrueHR <- rep(NA, DesignParam$NumTreatments)
    
    for(trmt in 1:DesignParam$NumTreatments) {
     # Createing pairwise data for estimating Hazard Ratios
     SimDataTrmt              <- SimData[SimData$TreatmentID %in% c(0, trmt),]
     
     # Compute Observed HR
     coxModel                 <- coxph(Surv(ObservedTime, Event) ~ TreatmentID, data = SimDataTrmt)
     vTrueHR[trmt]            <- exp(coxModel$coefficients)
    }

    # Step 4: Selecting the best 'QtyOfArmsToSelect' treatment arms with better HRs ####
    if(TailType == 0) {
      vReturnTreatmentID <- order(vTrueHR)[1:UserParam$QtyOfArmsToSelect]
    } else {
      vReturnTreatmentID <- order(vTrueHR, decreasing = TRUE)[1:UserParam$QtyOfArmsToSelect]
    }
    
    # Step 5: Readjusting the allocation ratio of the selected treatment arms ####
    if(is.null(UserParam$UpdatedAllocationRatio)) {
     vAllocationRatio <- DesignParam$InitialAllocInfo[vReturnTreatmentID]
    } else {
     vAllocationRatio <- UserParam$UpdatedAllocationRatio
    }

    nErrrorCode <- 0
    
    # Final Validation: The length( vReturnTreatmentID ) must equal length( vAllocationRatio ) ####
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #Fatal error because the R code is incorrect
        nErrrorCode <- -1
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
    
}


# # Calling example
# UserParam <- list(QtyOfArmsToSelect=3, UpdatedAllocationRatio = c(2, 1.5,1))
# SelectSpecifiedNumberOfExpWithBetterHRs(SimData=debugdata$SimData, DesignParam=debugdata$DesignParam, 
#                                         LookInfo=debugdata$LookInfo, UserParam = UserParam)
