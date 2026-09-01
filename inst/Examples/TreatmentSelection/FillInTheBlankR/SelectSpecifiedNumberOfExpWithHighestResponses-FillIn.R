######################################################################################################################## .
#' @name SelectSpecifiedNumberOfExpWithHighestResponses
#' @title Select a Specified Number of Highest-Response Treatments
#' @description
#' Provides a fill-in exercise for selecting the requested number of experimental
#' arms with the largest response counts and assigning rank-specific allocation ratios.
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
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'   \describe{
#'     \item{UserParam$QtyOfArmsToSelect}{Number of experimental arms selected to advance.}
#'     \item{UserParam$Rank1AllocationRatio, UserParam$Rank2AllocationRatio, ..., UserParam$RankNAllocationRatio}{Allocation ratio relative to control for the arm with response rank 1 through `N`, where `N` is `UserParam$QtyOfArmsToSelect`.}
#'   }
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectSpecifiedNumberOfExpWithHighestResponses <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    if( is.null( UserParam ) )
    {
        UserParam <- list( maxSelection = 2, highestResponse = 2, nextHighestResponse = 1 )
    }
    # Calculate the number of responses per arm and select the highest user-specified number (maxSelection) of arms
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Want to select the top user-specified (maxSelection) number of experimental treatments, so drop control from the sorting
    # Now, only the experimental treatments are left
    tabResults   <- ______[ -1, ]

    # Sort in descending order based on the number of responses (column 2)
    # After the sort, the matrix will have the largest number of responses in the first row and the smallest number of responses in the last row

    mSortedMatrix      <- tabResults[ order( tabResults[ , 2 ], decreasing = TRUE ), ]
    # Select the user-specified (maxSelection) number of treatments with the largest number of responses
    vReturnTreatmentID <- as.integer( row.names( mSortedMatrix[ 1:UserParam$max, ] ) )

    # The treatment with the highest number of responses should receive the user-specified highestResponse times as many patients as the next highest.
    # The allocation will put user-specified highestResponse times as many patients on the treatment with the highest number of responses
    # eg the treatment vReturnTreatmentID[ 1 ] will receive user-specified highestResponse times as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- c( ______, ______ )

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$highestResponse, vReturnTreatmentID[ 2 ] a ratio of UserParam$nextHighestResponse, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        # Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( ______ = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
