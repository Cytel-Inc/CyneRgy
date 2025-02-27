######################################################################################################################## .
#' Compare treatment and experimental to control with a chi-squared test, selecting treatments with a p-value less than specified value. 
#' @param SimData Dataframe which consists of data generated in current simulation
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#'  If UserParam is supplied, the list must contain the following named element:
#'  \describe{
#'  \item{UserParam$dMaxPValue}{A value (0,1) that defines the comparison chi-squared probability for selecting which treatments to advance. 
#'       Any treatment with less than the specified p-value will be advanced to the second stage}
#'           }
#' @description
#' At the interim analysis, compare treatment and each experimental to control using a chi-squared test. 
#' Any treatment with p-value < dMaxPValue is selected for stage 2.
#' If none of the treatments have a p-value < dMaxPValue, select the treatment with the smallest p-value
#' In the second stage, the randomization ratio will be 1:1 (experimental:control)
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
#'       Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2. 
#'       vAllocationRatio    <- c( 2, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'       
#'      Example 2: Assuming the allocation in 2nd part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 2 will receive twice as many as Experimental 1 or Control. 
#'       vAllocationRatio    <- c( 1, 2 )
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
######################################################################################################################## .

SelectExpWithPValueLessThanSpecified  <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    if( is.null( UserParam ) )
    {
        UserParam <- list( dMaxPValue = 0 )
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