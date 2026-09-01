######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Select Treatments at an Interim Analysis
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value specifying the index of arms to which subjects are allocated (one arm index per subject). Index for control is 0}
#'          \item{Response}{An integer value where 1 indicates response and 0 indicates no response.}
#'          \item{CensorInd}{An integer value indicating whether the subject was censored or not.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Integer. Sample size of the trial}
#'          \item{Alpha}{Numeric. Type I Error}
#'          \item{TrialType}{Integer. Type of the Trial. Values are Superiority: 0}
#'          \item{TestType}{Integer. Values are One side: 0}
#'          \item{TailType}{Integer. Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{InitialAllocInfo}{Vector of the ratios of the treatment group sample sizes to control group sample size. Length = number of treatment arms.}
#'          \item{VarType}{Integer. Variance Type. Values are Pooled: 0, Unpooled: 1}
#'          \item{TestID}{Integer. Test ID. Values are Difference of Proportions: 303}
#'          \item{MultAdjMethod}{Integer. Multiple Comparison Procedure. Values are Bonferroni: 0, Weighted Bonferroni: 2, Hochberg's Step Up: 4, Fixed Sequence: 6, Fallback: 7}
#'          \item{NumTreatments}{Integer. Number of Treatment arms}
#'          \item{AlphaProp}{Vector of Proportions of Alpha for each treatment arm}
#'          \item{TestSeq}{Vector of integer Test Sequence for each comparison which corresponds to each treatment arm.}
#'          \item{MaxCompleters}{Integer. Maximum Number of Completers.}
#'          \item{CriticalPoint}{Numeric. Critical Value for a fixed sample design.}
#'          \item{RespLag}{Numeric. Follow up duration.}
#'          \item{IsArmPresent}{Vector of integer flags indicating whether an arm is still present in the trial or was dropped in the interim. Length = number of treatment arms. Values are - Dropped in the interim: 0, Still present in the trial: 1}
#'          \item{UpdatedAllocInfo}{Vector of ratios of the treatment group sample sizes to control group sample size which may have been updated during treatment selection. Length = number of treatment arms.}
#'
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study.}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1.}
#'                      \item{CumCompleters}{Vector of Cumulative number of completer for all non time-to-event studies. Length = Number of looks.}
#'                      \item{InfoFrac}{Vector of numeric Information fraction. Length = Number of looks.}
#'                      \item{CumAlpha}{Vector of numeric Cumulative alpha spent, one sided tests. Length = Number of looks.}
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Integer. Efficacy boundary scale. Possible values are: Z Scale: 0}
#'                      \item{EffBdry}{Vector of numeric efficacy boundaries, one sided tests. Length = Number of looks.}
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Integer. Futility boundary scale. Possible value are: Delta Scale: 2}
#'                      \item{FutBdry}{Vector of numeric futility boundaries, one sided tests. Length = Number of looks.}
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{RejType}{Integer. Rejection Type. Values are: 1 Sided Efficacy Upper: 0, 1 Sided Futility Upper: 1, 1 Sided Efficacy Lower: 2, 1 Sided Futility Lower: 3, 1 Sided Efficacy Upper Futility Lower: 4, 1 Sided Efficacy Lower Futility Upper: 5}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @description
#' This function is used for the MAMS binary design and will perform treatment selection at the interim analysis (IA).
#' The example R code below will guide you through what needs to be done. Step 1, Step 2, Step 3, and Step 4 comments are added below
#' to help you find the most likely places to add your new R code.
#' @return A list containing:
#'   \describe{
#'     \item{TreatmentID}{Vector of experimental treatment IDs selected and carried forward.}
#'     \item{AllocRatio}{Vector of allocation ratios for the selected experimental treatments.}
#'     \item{ErrorCode}{Integer status code: 0 indicates no error, a positive value aborts the current simulation,
#'       and a negative value prevents further simulations.}
#'   }
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#' @examples  Example Output Object:
#'       Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#'      vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2.
#'       vAllocationRatio    <- c( 2, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments,
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'
#'      Example 2: Assuming the allocation in 2nd part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 2 will receive twice as many as Experimental 1 or Control.
#'       vAllocationRatio    <- c( 1, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments,
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'
######################################################################################################################## .

{{FUNCTION_NAME}}  <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # Pulling the important information from the simulated data, SimData, sent from East Horizon
    vTreatmentID    <- SimData$TreatmentID  # TreatmentIDs are 0, 1,..., number of experimental treatments
    vPatientOutcome <- SimData$Response     # Response = 0 or 1

    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

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
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        # Fatal error because the R code is incorrect.
        nErrorCode <- -1
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
