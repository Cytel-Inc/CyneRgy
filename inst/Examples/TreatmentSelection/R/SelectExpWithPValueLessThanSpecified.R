######################################################################################################################## .
#' @name SelectExpWithPValueLessThanSpecified
#' @title Select Treatments Using Chi-Squared P-Values
#' @description
#' Compares each experimental arm with control using a chi-squared test and selects
#' arms below the user-defined p-value threshold, with a smallest-p-value fallback.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Dataframe which consists of data generated in current simulation
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#'  If UserParam is supplied, the list must contain the following named element:
#'  \describe{
#'  \item{UserParam$dMaxPValue}{A value (0,1) that defines the comparison chi-squared probability for selecting which treatments to advance.
#'       Any treatment with less than the specified p-value will be advanced to the second stage}
#'           }
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpWithPValueLessThanSpecified <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
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
        if( is.nan( vPValue[ nIndex - 1 ] ) )
        {
            # The Chi Squared Test did not calculate a p-value, which can occur if no patients respond, so make the p-value 1
            vPValue[ nIndex - 1 ] <- 1
        }

        # Step 2 - Create the vector of selected treatments, with p-value < dMaxPValue ####
        if( vPValue[ nIndex - 1 ]  < as.numeric( UserParam$dMaxPValue ) )
        {
            # Note: the TreatmentID is nIndex - 1
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex - 1 )
        }
    }

    # If none of the experimental treatments had p-value < dMaxPValue, select the treatment with the smallest p-value
    if( length( vReturnTreatmentID ) == 0 )
    {
        vReturnTreatmentID <-  which.min( vPValue )
    }

    # Step 3: Create the allocation ratios for all selected treatments ####
    # In this case, all selected treatments should have an allocation ration of 1:1
    # The allocation will put twice as many patients on the treatment with the highest number of responses
    vAllocationRatio   <- rep( 1, length( vReturnTreatmentID ) )

    nErrorCode <- 0
    # Note: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
