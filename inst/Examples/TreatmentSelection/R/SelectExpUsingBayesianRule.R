######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#' @title Select experimental treatments for the next stage
#' @description This function is used for the MAMS design with a binary outcome and will perform treatment
#'   selection at the interim analysis (IA). At the IA, utilize a Bayesian rule to select any experimental
#'   treatment that has at least a user-specified probability (UserParam$dMinPosteriorProbability) of being greater
#'   than a user-specified historical response rate (UserParam$dHistoricResponseRate). Specifically, if Pr( pj >
#'   UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, then experimental treatment j
#'   is selected for stage 2. If none of the treatments meet the criteria for selection, then select the treatment
#'   with the largest Pr( pj > UserParam$dHistoricResponseRate | data ). User-specified pj ~ Beta(
#'   UserParam$dPriorAlpha, UserParam$dPriorBeta ). All experimental arms assume the same prior. After the IA, use
#'   a randomization ratio of 2:1 (experimental:control) for all experimental treatments that are selected for
#'   stage 2.
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
#'   \item{UserParam$dPriorAlpha}{Positive numeric alpha parameter of the beta prior. When UserParam is NULL,
#'     the default is 0.2.}
#'   \item{UserParam$dPriorBeta}{Positive numeric beta parameter of the beta prior. When UserParam is NULL,
#'     the default is 0.8.}
#'   \item{UserParam$dHistoricResponseRate}{Numeric historical response probability in [0, 1]. When UserParam
#'     is NULL, the default is 0.2.}
#'   \item{UserParam$dMinPosteriorProbability}{Numeric posterior probability threshold in [0, 1] for
#'     selecting a treatment whose response probability exceeds the historical rate. When UserParam is NULL,
#'     the default is 0.5.}
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
######################################################################################################################## .

SelectExpUsingBayesianRule <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed
    #   data in ‘SimData’ and the
    #       prior Beta (UserParam$dPriorAlpha, UserParam$dPriorBeta) distribution. Denote the number of patients on
    #   treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( UserParam$dPriorAlpha + Yj, UserParam$dPriorBeta + Y'j  )
    # 2)    Determine whether any experimental treatment has at least a UserParam$dMinPosteriorProbability chance pj >
    #   UserParam$dHistoricResponseRate, eg for any treatment j if Pr( pj > UserParam$dHistoricResponseRate | data ) >
    #   UserParam$dMinPosteriorProbability, select treatment j for stage 2.
    # 3)    If none of the treatments meet the above criteria for selection, then select the treatment with the largest
    #   Pr( pj > UserParam$dHistoricResponseRate | data ).
    # 4)    After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all
    #   experimental treatments that are selected for stage 2

    # The below lines set the values of the parameters if a user does not specify a value

    if ( is.null( UserParam ) ) {
        UserParam <- list( dPriorAlpha = 0.2, dPriorBeta = 0.8,
                           dHistoricResponseRate = 0.2, dMinPosteriorProbability = 0.5 )
    }

    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j)
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column,
    #   and number of responses is the second column.
    tabResults <- table( SimData$TreatmentID, factor( SimData$Response, levels = c( 0, 1 ) ) )

    # Use only the experimental treatments, whose data start in row 2.
    tabResultsExperimental <- tabResults[ c( 2:nrow( tabResults ) ), , drop = FALSE ]
    nQtyOfExperimentalArms <- nrow( tabResultsExperimental )
    vExperimentalTreatmentID <- as.integer( row.names( tabResultsExperimental ) )

    # Loop over the experimental arms and record which treatments are selected for stage 2
    vReturnTreatmentID <- c( )
    # Initialize the vector to keep vPostProbGreaterThanHistory. If none of the Post Prob >
    #   UserParam$dMinPosteriorProbability, the max can be selected from it
    vPostProbGreaterThanHistory <- rep( 0, nQtyOfExperimentalArms )

    for ( nArmIndex in seq_len( nQtyOfExperimentalArms ) ) {
        # Step 1: Compute the posterior parameters
        #           dPostAlpha = UserParam$dPriorAlpha + # Responses
        #           dPostBeta  = UserParam$dPriorBeta + # Treatment failures
        # Column 2 is the number of responses
        dPostAlpha <- UserParam$dPriorAlpha + tabResultsExperimental[ nArmIndex, 2 ]
        # Column 1 is the number of treatment failures
        dPostBeta <- UserParam$dPriorBeta + tabResultsExperimental[ nArmIndex, 1 ]

        # Step 2: Compute and store the posterior probability Prob( pi > UserParam$dHistoricResponseRate | data )
        vPostProbGreaterThanHistory[ nArmIndex ] <- 1 - stats::pbeta(
            UserParam$dHistoricResponseRate, dPostAlpha, dPostBeta
        )

        # Step 3: Did the posterior probability meet the criteria for selecting the treatment? Is Pr( pj >
        #   UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability?
        #         If so, add it to the list of treatments to select for stage 2
        if ( vPostProbGreaterThanHistory[ nArmIndex ] > UserParam$dMinPosteriorProbability ) {
            vReturnTreatmentID <- c( vReturnTreatmentID, vExperimentalTreatmentID[ nArmIndex ] )
        }
    }
    # Step 4: If none of the experimental treatments had a response rate greater than control, select the treatment with
    #   the largest response rate
    # No treatments met the criteria for selection so use the one with the largest Prob( pi >
    #   UserParam$dHistoricResponseRate | data )
    if ( length( vReturnTreatmentID ) == 0 ) {
        vReturnTreatmentID <- vExperimentalTreatmentID[ which.max( vPostProbGreaterThanHistory ) ]
    }

    # Set the allocation ratio
    # Use an allocation ratio of 2:1 for all selected treatments.
    vAllocationRatio <- rep( 2, length( vReturnTreatmentID ) )

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( vReturnTreatmentID ) != length( vAllocationRatio ) ) {
        nErrorCode <- -1 #  Fatal error because the R code is incorrect
    }

    lReturn <- list(
        TreatmentID = as.integer( vReturnTreatmentID ),
        AllocRatio = as.double( vAllocationRatio ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
