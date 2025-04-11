######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#' @title Select Experimental Treatments Using a Bayesian Rule
#' 
#' @description 
#' This function implements the MAMS design for binary outcomes and performs treatment selection at the interim analysis (IA) using a Bayesian decision rule. 
#' At IA, an experimental treatment is selected for stage 2 if its posterior probability of exceeding a user-specified historical response rate 
#' (`UserParam$dHistoricResponseRate`) is greater than a user-defined threshold (`UserParam$dMinPosteriorProbability`):  
#' `Pr(pj > UserParam$dHistoricResponseRate | data) > UserParam$dMinPosteriorProbability`. 
#' If no treatment satisfies this criterion, the treatment with the highest posterior probability is selected. 
#' All experimental arms assume the same prior distribution: `pj ~ Beta(UserParam$dPriorAlpha, UserParam$dPriorBeta)`. 
#' For stage 2, selected treatments are randomized against the control arm in a 2:1 ratio (experimental:control).
#' 
#' @param SimData Dataframe containing data generated in the current simulation.
#' @param DesignParam List of design and simulation parameters required to perform treatment selection.
#' @param LookInfo List containing design and simulation parameters, which might be required to perform treatment selection.
#' @param UserParam A list of user-defined parameters in East or East Horizon. The default is NULL.
#'  The list must contain the following named elements:
#'  \describe{
#'  \item{UserParam$dPriorAlpha}{A value (0,1) defining the prior alpha parameter of the beta distribution.}
#'  \item{UserParam$dPriorBeta}{A value (0,1) specifying the prior beta parameter of the beta distribution.}
#'  \item{UserParam$dHistoricResponseRate}{A value (0,1) specifying the historic response rate.}
#'  \item{UserParam$dMinPosteriorProbability}{A value (0,1) specifying the posterior probability needed to exceed the historic response rate for experimental treatment selection.}
#'  }
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
#' @export
######################################################################################################################## .

SelectExpUsingBayesianRule  <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the 
    #       prior Beta (UserParam$dPriorAlpha,UserParam$UserParam$dPriorBeta) distribution.  Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( UserParam$dPriorAlpha + Yj, UserParam$dPriorBeta + Y'j  )
    # 2)	Determine whether any experimental treatment has at least a UserParam$dMinPosteriorProbability chance pj > UserParam$dHistoricResponseRate, eg for any treatment j if Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, select treatment j for stage 2.
    # 3)	If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > UserParam$dHistoricResponseRate | data ).
    # 4)	After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2
    
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dPriorAlpha", "dPriorBeta", "dHistoricResponseRate", "dMinPosteriorProbability" )
    vMissingParams <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( TreatmentID  = as.integer( 0 ), 
                      ErrorCode    = as.integer( -1 ), 
                      AllocRatio   = as.double( 0 ) ) )
    }
    
    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j) 
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults               <- table( SimData$TreatmentID, SimData$Response )
    
    # Only want data on experimental treatments is wanted, experimental data starts in row 2
    tabResultsExperimental   <- tabResults[ c( 2:nrow( tabResults ) ), ]  
    nQtyOfExperimentalArms   <- nrow( tabResultsExperimental ) 
    
    # Loop over the experimental arms and record which treatments are selected for stage 2
    vReturnTreatmentID      <- c()
    # Initialize the vector to keep vPostProbGreaterThanHistory. If none of the Post Prob > UserParam$dMinPosteriorProbability, the max can be selected from it
    vPostProbGreaterThanHistory <- rep( 0, nQtyOfExperimentalArms )  
    
    for( iArm in 1:nQtyOfExperimentalArms )
    {
        # Step 1: Compute the posterior parameters 
        #           dPostAlpha = UserParam$dPriorAlpha + # Responses
        #           dPostBeta  = UserParam$dPriorBeta + # Treatment failures
        # Column 2 is the number of responses
        dPostAlpha <- UserParam$dPriorAlpha + tabResultsExperimental[ iArm, 2 ] 
        # Column 1 is the number of treatment failures
        dPostBeta  <- UserParam$dPriorBeta  + tabResultsExperimental[ iArm, 1 ]   
        
        # Step 2: Compute and store the posterior probability Prob( pi > UserParam$dHistoricResponseRate | data )
        vPostProbGreaterThanHistory[ iArm ] <- 1 - pbeta( UserParam$dHistoricResponseRate, dPostAlpha, dPostBeta )
        
        # Step 3: Did the posterior probability meet the criteria for selecting the treatment? Is Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability?
        #         If so, add it to the list of treatments to select for stage 2
        if( vPostProbGreaterThanHistory[ iArm ] > UserParam$dMinPosteriorProbability )
            vReturnTreatmentID <- c( vReturnTreatmentID, iArm )
    }
    # Step 4: If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate 
    # No treatments met the criteria for selection so use the one with the largest Prob( pi > UserParam$dHistoricResponseRate | data )
    if( length( vReturnTreatmentID ) == 0 ) 
    {
        vReturnTreatmentID <-  which.max( vPostProbGreaterThanHistory )
    }
    
    # Set the allocation ratio
    # We want to allocation ratio to be 2:1 for all selected treatments 
    vAllocationRatio   <- rep( 2, length( vReturnTreatmentID ) )    
    
    nErrrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        nErrrorCode <- -1  #  Fatal error because the R code is incorrect
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
}