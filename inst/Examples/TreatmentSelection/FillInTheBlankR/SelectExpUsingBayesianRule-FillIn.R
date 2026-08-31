######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#' @title Select Treatments Using a Bayesian Response Rule
#' @description
#' Provides a fill-in exercise for selecting experimental arms by posterior
#' probability, with a best-arm fallback.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'   `historicResponseRate`, and `treatmentPValue`.
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpUsingBayesianRule <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the
    #       prior Beta (dPriorAlpha,dPriorBeta) distribution.  Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( dPriorAlpha + Yj, dPriorBeta + Y'j  )
    # 2)    Determine whether any experimental treatment has at least a treatmentPValue chance pj > historicResponseRate, eg for any treatment j if Pr( pj > historicResponseRate | data ) > treatmentPValue, select treatment j for stage 2.
    # 3)    If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > historicResponseRate | data ).
    # 4)    After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2

    # The below lines set the values of the parameters if a user does not specify a value

    if( is.null( UserParam ) )
    {
        UserParam <- list( dPriorAlpha = 0.2, dPriorBeta = 0.8, historicResponseRate = 0.1, treatmentPValue = 0.2 )
    }

    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j)
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults               <- table( SimData$TreatmentID, SimData$Response )

    # Only want data on experimental treatments is wanted, experimental data starts in row 2
    tabResultsExperimental   <- tabResults[ 2:nrow( __________ ), ]
    nQtyOfExperimentalArms   <- nrow( tabResultsExperimental )

    # Loop over the experimental arms and record which treatments are selected for stage 2
    vReturnTreatmentID      <- c()
    # Initialize the vector to keep vPostProbGreaterThanHistory. If none of the Post Prob > treatmentPValue, the max can be selected from it
    vPostProbGreaterThanHistory <- rep( 0, ______________ )

    for( iArm in 1:nQtyOfExperimentalArms )
    {
        # Step 1: Compute the posterior parameters
        #           dPostAlpha = dPriorAlpha + # Responses
        #           dPostBeta  = dPriorBeta + # Treatment failures
        # Column 2 is the number of responses
        dPostAlpha <- UserParam$dPriorAlpha + tabResultsExperimental[ iArm, 2 ]
        # Column 1 is the number of treatment failures
        dPostBeta  <- UserParam$dPriorBeta  + ____________________[ iArm, 1 ]

        # Step 2: Compute and store the posterior probability Prob( pi > historicResponseRate | data )
        vPostProbGreaterThanHistory[ iArm ] <- 1 - pbeta( UserParam$historicResponseRate, dPostAlpha, dPostBeta )

        # Step 3: Did the posterior probability meet the criteria for selecting the treatment? Is Pr( pj > historicResponseRate | data ) > treatmentPValue?
        #         If so, add it to the list of treatments to select for stage 2
        if( vPostProbGreaterThanHistory[ iArm ] > UserParam$treatmentPValue )
            vReturnTreatmentID <- c( vReturnTreatmentID, iArm )

    }
    # Step 4: If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    # No treatments met the criteria for selection so use the one with the largest Prob( pi > historicResponseRate | data )
    if( length( vReturnTreatmentID ) == 0 )
    {
        vReturnTreatmentID <- which.max( ___________________________ )
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

    lReturn <- list( TreatmentID  = as.integer( vReturnTreatmentID ),
                     _____________ = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
