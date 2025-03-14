######################################################################################################################## .
#' @title Select Experimental Treatments Using P-value Comparison
#' 
#' @description
#' At the interim analysis, experimental treatments are compared to the control using a chi-squared test. Treatments with p-values 
#' less than `dMaxPValue` are selected for stage 2. If no treatments meet the threshold, the treatment with the smallest p-value is selected. 
#' In the second stage, the randomization ratio will be 1:1 (experimental:control).
#' 
#' @param SimData Dataframe containing data generated in the current simulation.
#' @param DesignParam List of design and simulation parameters required for treatment selection.
#' @param LookInfo List containing design and simulation parameters that might be required for treatment selection.
#' @param UserParam A list of user-defined parameters in East or East Horizon. Default is NULL. 
#'   The list must contain the following named element:
#'   \describe{
#'     \item{UserParam$dMaxPValue}{A value (0,1) specifying the chi-squared probability threshold for selecting treatments to advance. 
#'     Treatments with p-values less than this threshold will advance to the second stage.}
#'   }
#' 
#' @return A list containing:
#'   \item{TreatmentID}{A vector of experimental treatment IDs selected to advance, e.g., 1, 2, ..., number of experimental treatments.}
#'   \item{AllocRatio}{A vector of allocation ratios for the selected treatments relative to control.}
#'   \item{ErrorCode}{An integer indicating success or error status:
#'     \describe{
#'       \item{ErrorCode = 0}{No error.}
#'       \item{ErrorCode > 0}{Nonfatal error, current simulation aborted but subsequent simulations will run.}
#'       \item{ErrorCode < 0}{Fatal error, no further simulations attempted.}
#'     }
#'   }
#' 
#' @note 
#' \itemize{
#' \item The length of `TreatmentID` and `AllocRatio` must be the same.
#' \item The allocation ratio for control is always 1, and `AllocRatio` values are relative to this. For example, an allocation value of 2 means twice as many participants are randomized to the experimental treatment compared to control.
#' \item The order of `AllocRatio` should match `TreatmentID`, with corresponding elements assigned their respective allocation ratios.
#' \item The returned vector includes only `TreatmentID` values for experimental treatments. For example, `TreatmentID = c(0, 1, 2)` is invalid because control (`0`) should not be included.
#' \item At least one treatment and one allocation ratio must be returned.
#' }
#' 
#' @examples 
#' # Example 1: Allocation in the second stage is 1:2:2 for Control:Experimental 1:Experimental 2
#' vSelectedTreatments <- c(1, 2)  # Experimental 1 and Experimental 2 both have an allocation ratio of 2.
#' vAllocationRatio    <- c(2, 2)
#' nErrorCode          <- 0
#' lReturn             <- list(TreatmentID = vSelectedTreatments, 
#'                              AllocRatio  = vAllocationRatio,
#'                              ErrorCode   = nErrorCode)
#' return(lReturn)
#' 
#' # Example 2: Allocation in the second stage is 1:1:2 for Control:Experimental 1:Experimental 2
#' vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many patients as Experimental 1 or Control.
#' vAllocationRatio    <- c(1, 2)
#' nErrorCode          <- 0
#' lReturn             <- list(TreatmentID = vSelectedTreatments, 
#'                              AllocRatio  = vAllocationRatio,
#'                              ErrorCode   = nErrorCode)
#' return(lReturn)
#' @export
######################################################################################################################## .

SelectExpWithPValueLessThanSpecified  <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c("dMaxPValue")
    vMissingParams <- vRequiredParams[!vRequiredParams %in% names(UserParam)]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return(list(TreatmentID  = as.integer(0), 
                    ErrorCode    = as.integer(-1), 
                    AllocRatio   = as.double(0)))
    }
    
    # Calculate the number of responders and treatment failures for each treatment
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults   <- table( SimData$TreatmentID, SimData$Response )
    
    # Step 1 - The first step it to perform the data analysis to determine which treatments will be selected for stage 2 ####
    #           Since the chisq.test function requires a 2x2 table, the first row of tabResults can be taken for control and then treatment rows 2,3,4 can be looped through
    
    # This vector will be used to track which treatments have p-value < dMaxPValue and are then selected
    vReturnTreatmentID <- c() 
    # This vector will keep track of the p-values in the case that none are < dMaxPValue
    vPValue            <- c() 
    for( nIndex in 2:nrow( tabResults ) )
    {
        tabAnalysisData         <- tabResults[ c( 1, nIndex ), ]
        # Using nIndex  - 1 since there is not a p-value for control (nIndex = 1)
        vPValue[ nIndex - 1 ]   <- chisq.test( tabAnalysisData )$p.value    
        
        # Error checking - If the data had no patient responses, the p-value may not be able to be computed.
        if( is.nan( vPValue[ nIndex - 1 ] ))
        {
            # The Chi Squared Test did not calculate a p-value, which can occur if no patients respond, so make the p-value 1
            vPValue[ nIndex - 1 ] <- 1   
        }
        
        # Step 2 - Create the vector of selected treatments, with p-value < dMaxPValue ####
        if( vPValue[ nIndex - 1 ]  < as.numeric(UserParam$dMaxPValue))
        {
            # Note: the TreatmentID is nIndex - 1
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex - 1 )   
        }
    }
    
    # If none of the experimental treatments had p-value < dMaxPValue, select the treatment with the smallest p-value 
    if( length( vReturnTreatmentID ) == 0)
    {
        vReturnTreatmentID <-  which.min( vPValue  )
    }
    
    # Step 3: Create the allocation ratios for all selected treatments ####
    # In this case, all selected treatments should have an allocation ration of 1:1
    # The allocation will put twice as many patients on the treatment with the highest number of responses 
    vAllocationRatio   <- rep( 1, length( vReturnTreatmentID ) )    
    
    nErrorCode <- 0
    # Note: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1  
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )
    
    return( lReturn )
}