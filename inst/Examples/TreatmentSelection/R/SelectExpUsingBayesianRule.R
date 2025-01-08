######################################################################################################################## .
#' Select treatments to advance based on a Bayesian rule to select any that has at least a user-specified probability of being greater than a user-specified historical response rate. 
#' @param SimData Dataframe which consists of data generated in current simulation
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#'  If UserParam is supplied, the list must contain the following named element:
#'  \describe{
#'  \item {UserParam$dPriorAlpha} {A value (0,1) that defines the prior alpha parameter of the beta distribution. 
#'                          If this value is not specified, the default is 0.2.}  
#'  \item {UserParam$dPriorBeta} {A value (0,1) that specifies the prior beta parameter of the beta distribution. 
#'                              If this value is not specified, the default is 0.8.}
#'  \item {UserParam$dHistoricResponseRate} { A value (0,1) that specifies the historic response rate.
#'                                  If this value is not specified, the default is 0.2.}
#'  \item {UserParam$dMinPosteriorProbability} {A value (0,1) that specifies the posterior probability needed of being greater than the historic response rate for an experimental treatment to be selected. 
#'                              If this value is not specified, the default is 0.5.}
#'           }
#' @description
#' This function is used for the MAMS design with a binary outcome and will perform treatment selection at the interim analysis (IA).   
#' At the IA, utilize a Bayesian rule to select any experimental treatment that has at least a user-specified probability (UserParam$dMinPosteriorProbability) of being greater than a user-specified historical 
#' response rate (UserParam$dHistoricResponseRate). Specifically, if Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, then experimental treatment j is selected for stage 2.
#' If none of the treatments meet the criteria for selection, then select the treatment with the largest Pr( pj > UserParam$dHistoricResponseRate | data ).
#' User-specified pj ~ Beta( UserParam$dPriorAlpha, UserParam$dPriorBeta ). All experimental arms assume the same prior. 
#' After the IA, use a randomization ratio of 2:1 (experimental:control) for all experimental treatments that are selected for stage 2.

#' @return TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments
#' @return AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value. So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, e.g., TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#' @note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example. It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them. This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'       saveRDS( UserParam,   "UserParam.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
######################################################################################################################## .

SelectExpUsingBayesianRule  <- function(SimData, DesignParam, LookInfo, UserParam= NULL)
{
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the 
    #       prior Beta (UserParam$dPriorAlpha,UserParam$UserParam$dPriorBeta) distribution.  Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( UserParam$dPriorAlpha + Yj, UserParam$dPriorBeta + Y'j  )
    # 2)	Determine whether any experimental treatment has at least a UserParam$dMinPosteriorProbability chance pj > UserParam$dHistoricResponseRate, eg for any treatment j if Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, select treatment j for stage 2.
    # 3)	If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > UserParam$dHistoricResponseRate | data ).
    # 4)	After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2

          
    #Input objects can be saved through the following lines:
    #setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
    # The below lines set the values of the parameters if a user does not specify a value
    
    if( is.null( UserParam ) )
    {
        UserParam <- list(dPriorAlpha=0.2, dPriorBeta=0.8, dHistoricResponseRate=0.2, dMinPosteriorProbability = 0.5)
    }
    
    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j) 
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults               <- table( SimData$TreatmentID, SimData$Response )
   
     # Only want data on experimental treatments is wanted, experimental data starts in row 2
    tabResultsExperimental   <- tabResults[ c( 2:nrow( tabResults )), ]  
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
        vReturnTreatmentID <-  which.max( vPostProbGreaterThanHistory  )
    }
    
    # Set the allocation ratio
    # We want to allocation ratio to be 2:1 for all selected treatments 
    vAllocationRatio   <- rep( 2, length( vReturnTreatmentID ) )    
      
    nErrrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        nErrrorCode <- -1  #  Fatal error because the R code is incorrect
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
    
}
