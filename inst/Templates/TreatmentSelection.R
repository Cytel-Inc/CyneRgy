######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Select experimental treatments for the next stage
#'
#' @description Select experimental treatments for the next stage. Use this template as a starting point for custom
#'   logic. Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
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

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo, UserParam = NULL ) {
    # Pulling the important information from the simulated data, SimData, sent from East Horizon
    vTreatmentID <- SimData$TreatmentID # TreatmentIDs are 0, 1,..., number of experimental treatments
    vPatientOutcome <- SimData$Response # Response = 0 or 1

    # Step 1 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        # UserParam <- list( )
    }

    # Step 2: Perform any data analysis to decide which treatment(s) are selected ####

    # Add any code here for analysis

    # Step 3: Create the vector of experimental treatments that will continue to the next part of the trial ####
    # Example:
    # vReturnTreatmentID <- c( 1, 2 ) # Always select treatment 1 and 2

    # Add any code here for creating the treatment id vector

    # Step 4: Create a vector of allocation ratios ####
    # All ratios are relative to control, which has a ratio of 1, and are for the corresponding treatment in vReturnTreatmentID
    # Example: Put twice as many on experimental treatment 1 as there are on 2
    # vAllocationRatio   <- c( 2, 1 )    # This puts twice as many on Experimental treatment 1 because vReturnTreatmentID = c( 1, 2 ) in this example

    # Add any code necessary for creating the allocation ratio vector.

    # If you use the variable vReturnTreatmentID and vAllocationRatio above, then the remainder of this code will perform a basic error check
    # and create the return object.
    # Modify this code as necessary

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if ( length( vReturnTreatmentID ) != length( vAllocationRatio ) ) {
        # Fatal error because the R code is incorrect.
        nErrorCode <- -1
    }

    lReturn <- list(
        TreatmentID = as.integer( vReturnTreatmentID ),
        AllocRatio = as.double( vAllocationRatio ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
