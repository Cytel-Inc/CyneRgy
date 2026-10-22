######################################################################################################################## .
#' @name SelectExpWithPValueLessThanSpecified
#' @title Compare treatment and experimental to control with a chi-squared test, selecting treatments with a
#'   p-value less than specified value.
#' @description At the interim analysis, compare each experimental treatment to control using a chi-squared
#'   test. Any treatment with p-value < dMaxPValue is selected for stage 2. If none of the treatments have a
#'   p-value < dMaxPValue, select the treatment with the smallest p-value. In the second stage, the randomization
#'   ratio will be 1:1 (experimental:control).
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#' }
#' @param DesignParam Engine-supplied design parameters; not used by this selection rule.
#' @param LookInfo Engine-supplied analysis-look information; not used by this selection rule.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' If UserParam is supplied, the list must contain the following named element:
#'  \describe{
#'  \item{UserParam$dMaxPValue}{A value (0,1) that defines the comparison chi-squared probability for selecting
#'    which treatments to advance.
#'       Any treatment with less than the specified p-value will be advanced to the second stage}
#'           }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{TreatmentID}{Required integer vector of selected experimental-arm IDs, starting at 1 and excluding
#'     control. Return at least one selected arm.}
#'   \item{AllocRatio}{Required numeric vector of allocation ratios for the selected experimental arms, in the same
#'     order and with the same length as TreatmentID. Control has allocation 1.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details TreatmentID and AllocRatio must have the same nonzero length and matching order. Include only
#'   experimental treatment IDs; do not include 0 for control. The control allocation ratio is always 1.
#'
#' Worked output objects:
#' \preformatted{
#' # Example 1: Control:Experimental 1:Experimental 2 allocation is 1:2:2.
#' vSelectedTreatments <- c( 1, 2 )
#' vAllocationRatio <- c( 2, 2 )
#' lReturn <- list( TreatmentID = vSelectedTreatments,
#'                  AllocRatio = vAllocationRatio,
#'                  ErrorCode = 0L )
#' return( lReturn )
#'
#' # Example 2: Control:Experimental 1:Experimental 2 allocation is 1:1:2.
#' vSelectedTreatments <- c( 1, 2 )
#' vAllocationRatio <- c( 1, 2 )
#' lReturn <- list( TreatmentID = vSelectedTreatments,
#'                  AllocRatio = vAllocationRatio,
#'                  ErrorCode = 0L )
#' return( lReturn )
#' }
######################################################################################################################## .

SelectExpWithPValueLessThanSpecified <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    if ( is.null( UserParam ) ) {
        UserParam <- list( dMaxPValue = 0 )
    }

    # Calculate the number of responders and treatment failures for each treatment
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column,
    #   and number of responses is the second column.
    tabResults <- table( SimData$TreatmentID, factor( SimData$Response, levels = c( 0, 1 ) ) )

    # Step 1 - The first step is to perform the data analysis to determine which treatments will be selected for stage 2
    #   ####
    #           Since the chisq.test function requires a 2x2 table, the first row of tabResults can be taken for control
    #   and then treatment rows 2,3,4 can be looped through

    # This vector will be used to track which treatments have p-value < dMaxPValue and are then selected
    vReturnTreatmentID <- c( )
    # This vector will keep track of the p-values in the case that none are < dMaxPValue
    vPValue <- c( )
    vExperimentalTreatmentID <- as.integer( row.names( tabResults )[ -1 ] )
    for ( nIndex in 2:nrow( __________ ) ) {
        tabAnalysisData <- tabResults[ c( 1, nIndex ), ]
        # Using nIndex  - 1 since there is not a p-value for control (nIndex = 1)
        vPValue[ nIndex - 1 ] <- stats::chisq.test( tabAnalysisData )$p.value

        # Error checking - If the data had no patient responses, the p-value may not be able to be computed.
        if ( is.nan( vPValue[ nIndex - 1 ] ) ) {
            # The Chi Squared Test did not calculate a p-value, which can occur if no patients respond, so make the
            #   p-value 1
            vPValue[ nIndex - 1 ] <- 1
        }

        # Step 2 - Create the vector of selected treatments, with p-value < dMaxPValue ####
        if ( vPValue[ nIndex - 1 ] < __________ ) {
            # Use the actual TreatmentID from the table row name.
            vReturnTreatmentID <- c( vReturnTreatmentID, vExperimentalTreatmentID[ nIndex - 1 ] )
        }
    }

    # If none of the experimental treatments had p-value < dMaxPValue, select the treatment with the smallest p-value
    if ( length( __________ ) == 0 ) {
        __________ <- vExperimentalTreatmentID[ which.min( vPValue ) ]
    }

    # Step 3: Create the allocation ratios for all selected treatments ####
    # In this case, all selected treatments have an allocation ratio of 1:1
    vAllocationRatio <- rep( 1, length( vReturnTreatmentID ) )

    nErrorCode <- 0
    # Note: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( ___________________ ) != length( vAllocationRatio ) ) {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list(
        TreatmentID = as.integer( vReturnTreatmentID ),
        AllocRatio = as.double( vAllocationRatio ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
