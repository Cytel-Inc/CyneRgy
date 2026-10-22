######################################################################################################################## .
#' @name SelectSpecifiedNumberOfExpWithHighestResponses
#' @title Select experimental treatments for the next stage
#' @description This function is used for the MAMS design with a binary outcome and will perform treatment
#'   selection at the interim analysis (IA). At the IA, the user-specified number of experimental treatments
#'   (QtyOfArmsToSelect) that have the largest number of responses are selected. After the IA, we would like to
#'   randomize based on user-specified inputs: 1:Rank1AllocationRatio:Rank2AllocationRatio (control, selected
#'   experimental arm with highest number of responses, selected experimental arm with the second highest number of
#'   responses).
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
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$QtyOfArmsToSelect}{Integer number of experimental arms to advance, between 1 and the
#'     number of available experimental arms. When UserParam is NULL, the default is 1. Supply one
#'     RankNAllocationRatio parameter for each selected rank.}
#'   \item{UserParam$Rank1AllocationRatio}{Positive numeric treatment-to-control allocation ratio for the arm
#'     ranked first by number of responses. When UserParam is NULL, the default is 2.}
#'   \item{UserParam$Rank2AllocationRatio}{Positive numeric treatment-to-control allocation ratio for the arm
#'     ranked second by number of responses. Required when selecting at least two arms; there is no default.}
#'          }
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
#' @details Invalid selection counts or missing/non-positive allocation ratios return ErrorCode = -2.
#'
#' TreatmentID and AllocRatio must have the same nonzero length and matching order. Include only
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

SelectSpecifiedNumberOfExpWithHighestResponses <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    if ( is.null( UserParam ) ) {
        # Default is to select the treatment with highest number of responses and allocation of 2:1
        #   (Experimental:Control)
        UserParam <- list( QtyOfArmsToSelect = 1, Rank1AllocationRatio = 2 )
    }
    # Calculate the number of responses per arm and select the highest user-specified number (QtyOfArmsToSelect) of arms
    tabResults <- table( SimData$TreatmentID, factor( SimData$Response, levels = c( 0, 1 ) ) )

    # Want to select the top user-specified (QtyOfArmsToSelect) number of experimental treatments, so drop control from
    #   the sorting
    # Now, only the experimental treatments are left
    tabResults <- tabResults[ -1, , drop = FALSE ]

    nArmsToSelect <- UserParam$QtyOfArmsToSelect
    if ( length( nArmsToSelect ) != 1 || !is.finite( nArmsToSelect ) ||
         nArmsToSelect != as.integer( nArmsToSelect ) || nArmsToSelect < 1 ||
         nArmsToSelect > nrow( tabResults ) ) {
        return( list( TreatmentID = integer( 0 ), AllocRatio = numeric( 0 ), ErrorCode = -2L ) )
    }
    for ( nRank in seq_len( nArmsToSelect ) ) {
        dAllocationRatio <- UserParam[[ paste0( "Rank", nRank, "AllocationRatio" ) ]]
        if ( length( dAllocationRatio ) != 1 || !is.finite( dAllocationRatio ) || dAllocationRatio <= 0 ) {
            return( list( TreatmentID = integer( 0 ), AllocRatio = numeric( 0 ), ErrorCode = -2L ) )
        }
    }

    # Sort in descending order based on the number of responses (column 2)
    # After the sort, the matrix will have the largest number of responses in the first row and the smallest number of
    #   responses in the last row
    mSortedMatrix <- tabResults[ order( tabResults[ , 2 ], decreasing = TRUE ), , drop = FALSE ]

    # Select the user-specified (QtyOfArmsToSelect) number of treatments with the largest number of responses
    vSortedNames <- row.names( mSortedMatrix ) # Get the names of the treatments in order by number of responses
    vReturnTreatmentID <- as.integer( vSortedNames[ 1:UserParam$QtyOfArmsToSelect ] ) # Select the number of desired treatments.

    # Each selected arm receives its RankNAllocationRatio times as many patients as control.
    # NOTE: Always pull elements from the list by name rather than assuming a specific order
    vAllocationRatio <- c( )
    for ( nRank in seq_len( UserParam$QtyOfArmsToSelect ) ) {
        vAllocationRatio <- c( vAllocationRatio, UserParam[[ paste0( "Rank", nRank, "AllocationRatio" ) ]] )
    }

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$Rank1AllocationRatio and
    # vReturnTreatmentID[ 2 ] a ratio of UserParam$Rank2AllocationRatio, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( vReturnTreatmentID ) != length( vAllocationRatio ) ) {
        # Fatal error because the R code is incorrect
        nErrorCode <- -1
    }
    lReturn <- list(
        TreatmentID = as.integer( vReturnTreatmentID ),
        AllocRatio = as.double( vAllocationRatio ),
        ErrorCode = as.integer( nErrorCode )
    )
    return( lReturn )
}
