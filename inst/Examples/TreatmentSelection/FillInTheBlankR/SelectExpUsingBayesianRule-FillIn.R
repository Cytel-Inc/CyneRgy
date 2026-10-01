######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#'
#' @title Select treatments to advance based on a Bayesian rule to select any that has at least a user-specified
#'   probability of being greater than a user-specified historical response rate.
#'
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
#'
#' @author Sydney Ringold, J. Kyle Wathen
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{CensorInd}{Integer vector of censor indicators, with one element per subject: 0 = dropout/non-completer;
#'     1 = completer.}
#'   \item{CensorIndOrg}{Original integer vector of censor indicators before any analysis-time adjustment: 0 =
#'     dropout/non-completer; 1 = completer.}
#' }
#'
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TrialType}{Integer. Trial Type: – `0`: Superiority.}
#'   \item{TestType}{Integer. Test Type: – `0`: One-sided.}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed.}
#'   \item{InitialAllocInfo}{Vector of Numeric. Vector of length equal to the number of treatment arms (number of
#'     arms - 1), containing the ratios of the treatment group sample sizes to control group sample size.}
#'   \item{CriticalPoint}{Numeric. Critical value. East Horizon Explore: Only available if `Statistical Design =
#'     Fixed Sample`. Not available for `Study Objective = Dose Finding`. East Horizon Design: Not available for
#'     `Combining P-Values (MAMS)` tests.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial. Only available for time-to-event endpoints.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#'   \item{TestStatType}{Integer. Test statistic type: - `3`: Z-test. - `4`: t-test. East Horizon Explore: Only
#'     available for `Endpoint Type = Continuous`. East Horizon Design: Only available for `Continuous` tests.}
#'   \item{VarType}{Integer. Variance type. For `Continuous` test: - `4`: Equal - `5`: Unequal. For `Difference of
#'     Proportions` (Binary) test: - `0`: Pooled. `1`: Unpooled. East Horizon Explore: Not available for `Endpoint
#'     Type = Time-to-Event`. East Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{PValCombMethod}{Integer. P-value combination method: - `0`: Inverse normal. East Horizon Explore: Not
#'     available. East Horizon Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w1}{Numeric. Inverse normal weights for stage 1. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w2}{Numeric. Inverse normal weights for stage 2. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{MultAdjMethod}{Integer. Multiple comparison procedure. East Horizon Explore: Possible values: – `0`:
#'     Bonferroni. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. East Horizon Design: Possible values: – `0`: Bonferroni. – `1`: Sidak.
#'     – `2`: Simes. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. – `10`: Holm's Step Down. – `11`: Hommel's Step Up. – `12`: Dunnett's
#'     Step Down. – `13`: Dunnett's Step Up.}
#'   \item{NumTreatments}{Integer number of experimental treatment arms, excluding control.}
#'   \item{AlphaProp}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the proportion of Alpha for each treatment arm. East Horizon Explore: Only available for
#'     `Multiple Comparison Procedure = 4 (Weighted Bonferroni) or 6 (Fallback)`. Not available for `Study
#'     Objective = Dose Finding`. East Horizon Design: Only available for `Multiple Comparison Procedure = 4
#'     (Weighted Bonferroni) or 6 (Fallback)`. Not available for `Test = MAMS Difference of Means: Combining
#'     P-Values (Continuous) or MAMS Difference of Proportions: Combining P-Values (Binary) or MAMS Logrank
#'     (Time-to-Event)`.}
#'   \item{TestSeq}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the test sequence for each comparison (each treatment arm). East Horizon Explore: Only available
#'     for `Multiple Comparison Procedure = 5 (Fixed Sequence) or 6 (Fallback)`. Not available for `Study Objective
#'     = Dose Finding`. East Horizon Design: Only available for `Multiple Comparison Procedure = 5 (Fixed Sequence)
#'     or 6 (Fallback)`. Not available for `Test = MAMS Difference of Means: Combining P-Values (Continuous) or
#'     MAMS Difference of Proportions: Combining P-Values (Binary) or MAMS Logrank (Time-to-Event)`.}
#'   \item{IsArmPresent}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#'   \item{UpdatedAllocInfo}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the updated ratios of the treatment group sample sizes to control group sample size, which may
#'     have been updated during treatment selection.}
#' }
#'
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumAlpha}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the cumulative alpha spent
#'     (for one-sided tests) for each look. East Horizon Design: Only available if `Test Type = One-sided`.}
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number
#'     of completers for each look. East Horizon Explore: Not available for `Endpoint Type = Time-to-Event`. East
#'     Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{CumEvents}{Integer cumulative number of events at the current look. Only available for time-to-event
#'     endpoints.}
#'   \item{RejType}{Integer. Rejection type: – `0`: One-sided efficacy upper. – `1`: One-sided futility upper. –
#'     `2`: One-sided efficacy lower. – `3`: One-sided futility lower. – `4`: One-sided efficacy upper, futility
#'     lower. – `5`: One-sided efficacy lower, futility upper.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Not available for `Study Objective
#'     = Dose Finding`. Possible values: – `0`: Z scale. East Horizon Design: Possible values: `1`: Adjusted
#'     p-value scale.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values for each look. East Horizon Explore: Not available for `Study Objective = Dose Finding`.}
#'   \item{FutBdryScale}{Integer. Futility boundary scale. East Horizon Explore: Possible values: – `2`: Delta
#'     scale. – `6`: Hazard ratio scale. East Horizon Design: Possible values: – `1`: Adjusted p-value scale. –
#'     `2`: Delta scale. – `6`: Hazard ratio scale.}
#'   \item{FutBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the futility boundary
#'     values for each look.}
#' }
#'
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
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{TreatmentID}{Required integer vector of selected experimental-arm IDs, starting at 1 and excluding
#'     control. Return at least one selected arm.}
#'   \item{AllocRatio}{Required numeric vector of allocation ratios for the selected experimental arms, in the same
#'     order and with the same length as TreatmentID. Control has allocation 1.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
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

SelectExpUsingBayesianRule <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the
    #       prior Beta (UserParam$dPriorAlpha, UserParam$dPriorBeta) distribution. Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( UserParam$dPriorAlpha + Yj, UserParam$dPriorBeta + Y'j  )
    # 2)    Determine whether any experimental treatment has at least a UserParam$dMinPosteriorProbability chance pj > UserParam$dHistoricResponseRate, eg for any treatment j if Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability, select treatment j for stage 2.
    # 3)    If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > UserParam$dHistoricResponseRate | data ).
    # 4)    After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2

    # The below lines set the values of the parameters if a user does not specify a value

    if ( is.null( UserParam ) ) {
        UserParam <- list( dPriorAlpha = 0.2, dPriorBeta = 0.8, dHistoricResponseRate = 0.2, dMinPosteriorProbability = 0.5 )
    }

    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j)
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults <- table( SimData$TreatmentID, factor( SimData$Response, levels = c( 0, 1 ) ) )

    # Only want data on experimental treatments is wanted, experimental data starts in row 2
    tabResultsExperimental <- tabResults[ c( 2:nrow( __________ ) ), , drop = FALSE ]
    nQtyOfExperimentalArms <- nrow( tabResultsExperimental )
    vExperimentalTreatmentID <- as.integer( row.names( tabResultsExperimental ) )

    # Loop over the experimental arms and record which treatments are selected for stage 2
    vReturnTreatmentID <- c( )
    # Initialize the vector to keep vPostProbGreaterThanHistory. If none of the Post Prob > UserParam$dMinPosteriorProbability, the max can be selected from it
    vPostProbGreaterThanHistory <- rep( 0, ______________ )

    for ( iArm in 1:nQtyOfExperimentalArms ) {
        # Step 1: Compute the posterior parameters
        #           dPostAlpha = UserParam$dPriorAlpha + # Responses
        #           dPostBeta  = UserParam$dPriorBeta + # Treatment failures
        # Column 2 is the number of responses
        dPostAlpha <- UserParam$dPriorAlpha + tabResultsExperimental[ iArm, 2 ]
        # Column 1 is the number of treatment failures
        dPostBeta <- UserParam$dPriorBeta + ____________________[ iArm, 1 ]

        # Step 2: Compute and store the posterior probability Prob( pi > UserParam$dHistoricResponseRate | data )
        vPostProbGreaterThanHistory[ iArm ] <- 1 - stats::pbeta( UserParam$dHistoricResponseRate, dPostAlpha, dPostBeta )

        # Step 3: Did the posterior probability meet the criteria for selecting the treatment? Is Pr( pj > UserParam$dHistoricResponseRate | data ) > UserParam$dMinPosteriorProbability?
        #         If so, add it to the list of treatments to select for stage 2
        if ( vPostProbGreaterThanHistory[ iArm ] > UserParam$dMinPosteriorProbability ) {
            vReturnTreatmentID <- c( vReturnTreatmentID, vExperimentalTreatmentID[ iArm ] )
        }
    }
    # Step 4: If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    # No treatments met the criteria for selection so use the one with the largest Prob( pi > UserParam$dHistoricResponseRate | data )
    if ( length( vReturnTreatmentID ) == 0 ) {
        vReturnTreatmentID <- vExperimentalTreatmentID[ which.max( ___________________________ ) ]
    }

    # Set the allocation ratio
    # We want to allocation ratio to be 2:1 for all selected treatments
    vAllocationRatio <- rep( 2, length( vReturnTreatmentID ) )

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( vReturnTreatmentID ) != length( vAllocationRatio ) ) {
        nErrorCode <- -1 #  Fatal error because the R code is incorrect
    }

    lReturn <- list(
        TreatmentID = as.integer( vReturnTreatmentID ),
        _____________ = as.double( vAllocationRatio ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
