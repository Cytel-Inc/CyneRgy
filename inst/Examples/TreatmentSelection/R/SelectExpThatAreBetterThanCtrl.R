######################################################################################################################## .
#' @name SelectExpThatAreBetterThanCtrl
#' @title Select experimental treatments for the next stage
#' @description At the interim analysis, select any treatment with a response rate that is higher than control for
#'   stage 2. If none of the treatments have a higher response rate than control, select the treatment with the
#'   largest observed response rate. In the second stage, the randomization ratio will be 1:1
#'   (experimental:control).
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

SelectExpThatAreBetterThanCtrl <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    # Calculate the number of responders and treatment failures for each treatment

    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column,
    #   and number of responses is the second column.
    tabResults <- table( SimData$TreatmentID, factor( SimData$Response, levels = c( 0, 1 ) ) )

    # Compute the response probability as # of responses/(  # of treatment failures + # of responses )
    vProbabilityResponse <- as.vector( tabResults[ , 2 ] / ( tabResults[ , 1 ] + tabResults[ , 2 ] ) )

    # Create a variable with the probability of response on control to be used in decision making
    dProbabilityOfResponseOnControl <- vProbabilityResponse[ 1 ]
    # Create vector with only the estimated probability of response on experimentals
    vProbabilityResponseOnExperimental <- vProbabilityResponse[ c( 2:length( vProbabilityResponse ) ) ]

    vExperimentalTreatmentID <- as.integer( row.names( tabResults )[ -1 ] )

    # Note: vProbabilityResponseOnExperimental now contains only the response rates for the experimental treatments

    # Selection Rule: Any treatment with a response rate that is higher than control is selected for stage 2
    vReturnTreatmentID <- c( )
    # Note: Start with row 2, which is experimental treatment 1
    for ( nIndex in seq_along( vProbabilityResponseOnExperimental ) ) {
        # If the response rate > response rate on control, add the treatment ID to the list
        if ( vProbabilityResponseOnExperimental[ nIndex ] > dProbabilityOfResponseOnControl ) {
            vReturnTreatmentID <- c( vReturnTreatmentID, vExperimentalTreatmentID[ nIndex ] )
        }
    }

    # If none of the experimental treatments had a response rate greater than control, select the treatment with the
    #   largest response rate
    if ( length( vReturnTreatmentID ) == 0 ) {
        vReturnTreatmentID <- vExperimentalTreatmentID[ which.max( vProbabilityResponseOnExperimental ) ]
    }

    # Selected experimental arms have the same allocation as control.
    vAllocationRatio <- rep( 1, length( vReturnTreatmentID ) )
    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( vReturnTreatmentID ) != length( vAllocationRatio ) ) {
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
