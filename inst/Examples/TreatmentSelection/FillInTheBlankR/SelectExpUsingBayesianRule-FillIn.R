######################################################################################################################## .
#' @name SelectExpUsingBayesianRule
#' @title Select Treatments Using a Bayesian Response Rule
#' @description
#' Provides a fill-in exercise for selecting experimental arms by posterior
#' probability, with a best-arm fallback.
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
#'     \item{UserParam$dPriorAlpha}{First Beta-prior shape parameter for each experimental response probability.}
#'     \item{UserParam$dPriorBeta}{Second Beta-prior shape parameter for each experimental response probability.}
#'     \item{UserParam$dHistoricResponseRate}{Historical response rate that experimental treatments must exceed.}
#'     \item{UserParam$dMinPosteriorProbability}{Minimum posterior probability of exceeding the historical response rate required for selection.}
#'   }
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpUsingBayesianRule <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # Brief overview of what steps this function takes ####
    # 1)    For each experimental treatment j, calculate the posterior probability distribution based on the observed data in ‘SimData’ and the
    #       prior Beta (dPriorAlpha,dPriorBeta) distribution.  Denote the number of patients on treatment j by Nj, number of patient responses Yj, and the number of patients with treatment failure by
    #       Y'j = Nj - Yj the distribution pj | data ~ Beta( dPriorAlpha + Yj, dPriorBeta + Y'j  )
    # 2)    Determine whether any experimental treatment has at least a treatmentPValue chance pj > historicResponseRate, eg for any treatment j if Pr( pj > historicResponseRate | data ) > treatmentPValue, select treatment j for stage 2.
    # 3)    If none of the treatments meet the above criteria for selection, then select the treatment with the largest Pr( pj > historicResponseRate | data ).
    # 4)    After selecting the treatments, use a randomization ratio of 2:1 (experimental: control) for all experimental treatments that are selected for stage 2

    # The below lines set the values of the parameters if a user does not specify a value

    if( is.null( UserParam ) )
    {
        UserParam <- list( dPriorAlpha = 0.2, dPriorBeta = 0.8, historicResponseRate = 0.1, treatmentPValue = 0.2 )
    }

    #### Determine the posterior parameters based on SimData and the prior parameters ####
    # Calculate the number of responses (Yj) and treatment failures per treatment (Y'j)
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults               <- table( SimData$TreatmentID, SimData$Response )

    # Only want data on experimental treatments is wanted, experimental data starts in row 2
    tabResultsExperimental   <- tabResults[ 2:nrow( __________ ), ]
    nQtyOfExperimentalArms   <- nrow( tabResultsExperimental )

    # Loop over the experimental arms and record which treatments are selected for stage 2
    vReturnTreatmentID      <- c()
    # Initialize the vector to keep vPostProbGreaterThanHistory. If none of the Post Prob > treatmentPValue, the max can be selected from it
    vPostProbGreaterThanHistory <- rep( 0, ______________ )

    for( iArm in 1:nQtyOfExperimentalArms )
    {
        # Step 1: Compute the posterior parameters
        #           dPostAlpha = dPriorAlpha + # Responses
        #           dPostBeta  = dPriorBeta + # Treatment failures
        # Column 2 is the number of responses
        dPostAlpha <- UserParam$dPriorAlpha + tabResultsExperimental[ iArm, 2 ]
        # Column 1 is the number of treatment failures
        dPostBeta  <- UserParam$dPriorBeta  + ____________________[ iArm, 1 ]

        # Step 2: Compute and store the posterior probability Prob( pi > historicResponseRate | data )
        vPostProbGreaterThanHistory[ iArm ] <- 1 - pbeta( UserParam$historicResponseRate, dPostAlpha, dPostBeta )

        # Step 3: Did the posterior probability meet the criteria for selecting the treatment? Is Pr( pj > historicResponseRate | data ) > treatmentPValue?
        #         If so, add it to the list of treatments to select for stage 2
        if( vPostProbGreaterThanHistory[ iArm ] > UserParam$treatmentPValue )
            vReturnTreatmentID <- c( vReturnTreatmentID, iArm )

    }
    # Step 4: If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    # No treatments met the criteria for selection so use the one with the largest Prob( pi > historicResponseRate | data )
    if( length( vReturnTreatmentID ) == 0 )
    {
        vReturnTreatmentID <- which.max( ___________________________ )
    }

    # Set the allocation ratio
    # We want to allocation ratio to be 2:1 for all selected treatments
    vAllocationRatio   <- rep( 2, length( vReturnTreatmentID ) )

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        nErrorCode <- -1  #  Fatal error because the R code is incorrect
    }

    lReturn <- list( TreatmentID  = as.integer( vReturnTreatmentID ),
                     _____________ = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
