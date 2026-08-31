######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#' @title Select Treatments Using a Bayesian Response Rule
#' @description
#' Selects experimental arms whose posterior probability of exceeding a historical
#' response rate is above a user-defined threshold, with a best-arm fallback.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Dataframe which consists of data generated in current simulation
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL.
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
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpUsingBayesianRule <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the
    #       prior Beta (UserParam$dPriorAlpha,UserParam$UserParam$dPriorBeta) distribution.  Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( UserParam$dPriorAlpha + Yj, UserParam$dPriorBeta + Y'j  )
    # 2)    Determine whether any experimental treatment has at least a UserParam$dMinPosteriorProbability chance pj > UserParam$dHistoricResponseRate, eg for any treatment j if Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, select treatment j for stage 2.
    # 3)    If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > UserParam$dHistoricResponseRate | data ).
    # 4)    After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2

    # The below lines set the values of the parameters if a user does not specify a value

    if( is.null( UserParam ) )
    {
        UserParam <- list( dPriorAlpha=0.2, dPriorBeta=0.8, dHistoricResponseRate=0.2, dMinPosteriorProbability = 0.5 )
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

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        nErrorCode <- -1  #  Fatal error because the R code is incorrect
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
