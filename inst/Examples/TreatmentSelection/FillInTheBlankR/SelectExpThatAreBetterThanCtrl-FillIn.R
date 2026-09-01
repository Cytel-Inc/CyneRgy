######################################################################################################################## .
#' @name SelectExpThatAreBetterThanCtrl
#' @title Select Treatments with Response Rates Above Control
#' @description
#' Provides a fill-in exercise that selects experimental arms with observed response
#' rates above control, with a highest-response-rate fallback.
#' @author Sydney Ringold, J. Kyle Wathen
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
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpThatAreBetterThanCtrl <- function( SimData, DesignParam, LookInfo )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # Calculate the number of responders and treatment failures for each treatment

    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Compute the response probability as # of responses/(  # of treatment failures + # of responses )
    vProbabilityResponse            <- as.vector( ____________[ , 2 ] / ( tabResults[ , 1 ] + tabResults[ , 2 ] ) )
    # Create a variable with the probability of response on control to be used in decision making
    dProbabilityOfResponseOnControl     <- ____________[ 1 ]
    # Create vector with only the estimated probability of response on experimentals
    vProbabilityResponseOnExperimental  <- ____________[ 2:length( vProbabilityResponse ) ]

    # Note: vProbabilityResponseOnExperimental now contains only the response rates for the experimental treatments

    # Selection Rule: Any treatment with a response rate that is higher than control is selected for stage 2
    vReturnTreatmentID <- c()
    # Note: Start with row 2, which is experimental treatment 1
    for( nIndex in 1:length( vProbabilityResponseOnExperimental ) )
    {
        # If the response rate > response rate on control, add the treatment ID to the list
        if( vProbabilityResponseOnExperimental[ nIndex ] > _____________ )
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex )

    }

    # If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    if( length( ____________ ) == 0 )
    {
        vReturnTreatmentID <- which.max( vProbabilityResponseOnExperimental )
    }

    # We want all treatments to have a randomization ratio of 1
    # The allocation will put twice as many on the treatment with the highest number of responses,
    # eg. the Treatment vReturnTreatmentID[ 1 ] will receive twice as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- rep( 1, length( ____________ ) )

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of 2, vReturnTreatmentID[ 2 ] a ratio of 1, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
