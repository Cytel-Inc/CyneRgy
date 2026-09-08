######################################################################################################################## .
#' @name AnalyzeMultipleOutcomesCovariates
#' @title Analyze Simulated Patient Data with Multiple Independent Outcomes and Covariates
#'
#' @description This function performs statistical analysis on simulated patient-level data with multiple continuous endpoints and binary
#' covariates. For each endpoint, it fits an ANCOVA model to assess the treatment effect while adjusting for covariates.
#' The function returns binary decisions for treatment and covariate effects, along with group-wise means and sample sizes.
#' The output of this function is generated dynamically based on the number of covariates and endpoints, but it follows the same logic
#' as the explicitly constructed output list in `AnalyzeMultipleOutcomes.R`.
#' @author Julija Saltane
#'
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{PatientOutcome[X]}{Numeric vector representing results for endpoint X, where X = 1, 2, 3}
#'          \item{Covariate[Y]}{Binary vector representing results for covariate Y, where Y = 1, 2}
#'          \item{TreatmentID}{Integer vector (0 = control, 1 = treatment)}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'        \describe{
#'          \item{Alpha}{1-sided Type I Error. Note it will be internally converted to two-sided (i.e., 2 × Alpha) for ANCOVA.}
#'        }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'   \describe{
#'     \item{Fixed-sample support}{This example does not use multiple-look information; `LookInfo` should be `NULL`.}
#'   }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @return A list that contains:
#' \describe{
#'     \item{Decision}{An integer scalar placeholder with value 1.}
#'     \item{SampleSizeCtrl}{An integer scalar containing the number of patients assigned to control.}
#'     \item{SampleSizeTrt}{An integer scalar containing the number of patients assigned to treatment.}
#'     \item{MeanOutcome[X]Ctrl}{A numeric scalar containing the mean of endpoint X in the control group, for each endpoint X.}
#'     \item{MeanOutcome[X]Trt}{A numeric scalar containing the mean of endpoint X in the treatment group, for each endpoint X.}
#'     \item{DecisionOutcome[X]Trt}{An integer decision for the treatment effect on endpoint X: 1 indicates significance and 0 indicates non-significance.}
#'     \item{DecisionOutcome[X]Covariate[Y]}{An integer decision for covariate Y on endpoint X: 1 indicates significance and 0 indicates non-significance.}
#'     \item{PValueOutcome[X]Trt}{A numeric p-value for the treatment effect on endpoint X.}
#'     \item{PValueOutcome[X]Covariate[Y]}{A numeric p-value for covariate Y on endpoint X.}
#'     \item{ErrorCode}{An integer value: ErrorCode = 0 indicates no error; ErrorCode > 0 indicates a nonfatal error and aborts the current simulation, but subsequent simulations continue; ErrorCode < 0 indicates a fatal error and stops further simulation.}
#' }
#'
#' @examples
#'
#' # Simulate patient data with three independent outcomes and two covariates
#' UserParam   <- list(MeanOutcome1Ctrl = 10, MeanOutcome1Trt = 12,
#'                     MeanOutcome2Ctrl = 20, MeanOutcome2Trt = 22,
#'                     MeanOutcome3Ctrl = 30, MeanOutcome3Trt = 32,
#'                     Beta1 = 0.1, Beta2 = 2,
#'                     Cov1Prob = 0.2, Cov2Prob = 0.5)
#'
#' NumSub      <- 100
#' TreatmentID <- rep(c(0,1), NumSub / 2, replace = TRUE)
#'
#' response    <- SimulateMultipleOutcomesCovariates(NumSub = NumSub,
#'                                                   TreatmentID = TreatmentID,
#'                                                   Mean = NULL,
#'                                                   StdDev = NULL,
#'                                                   UserParam = UserParam)
#'
#' # Change the format of the simulated data (this is only required for testing the code in R, outside of East Horizon)
#' response_df <- as.data.frame(response[c("PatientOutcome1", "PatientOutcome2", "PatientOutcome3", "Covariate1", "Covariate2")])
#' response_df <- cbind(response_df, TreatmentID)
#'
#' # Analyze the simulated patient data
#' result <- AnalyzeMultipleOutcomesCovariates(SimData = response_df,
#'                                             DesignParam = list("Alpha" = 0.025))
#'
######################################################################################################################## .

AnalyzeMultipleOutcomesCovariates <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{

    # Extract 1-sided Type I error and transform it into two-sided for ANCOVA
    dAlphaOneSided   <- DesignParam$Alpha
    dAlphaTwoSided   <- dAlphaOneSided * 2

    # Extract outcomes and covariates
    lPatientOutcomes <- as.list( SimData[ , grep( "PatientOutcome", names( SimData ) ) ] )
    lCovariates      <- as.list( SimData[ , grep( "Covariate", names( SimData ) ) ] )

    # Determine number of endpoints and covariates
    nQtyOfEndpoints  <- length( lPatientOutcomes )
    nQtyOfCovariates <- length( lCovariates )

    # Create a vector of treatment assignments
    vTreatmentID     <- SimData$TreatmentID

    # Determine the number of patients in each group
    nSampleSizeCtrl  <- sum( vTreatmentID == 0 )
    nSampleSizeTrt   <- sum( vTreatmentID == 1 )

    # Initialize lists and vectors to store results for each endpoint
    lPValues         <- vector( "list", nQtyOfEndpoints )
    lDecision        <- vector( "list", nQtyOfEndpoints )

    vMeanOutcomeCtrl <- numeric( nQtyOfEndpoints )
    vMeanOutcomeTrt  <- numeric( nQtyOfEndpoints )

    # Run separate ANCOVA for each endpoint
    for( i in 1:nQtyOfEndpoints )
    {
        # Compute group means
        vMeanOutcomeCtrl[ i ]  <- mean( lPatientOutcomes[[ i ] ][ vTreatmentID == 0 ] )
        vMeanOutcomeTrt[ i ]   <- mean( lPatientOutcomes[[ i ] ][ vTreatmentID == 1 ] )

        # Build formula dynamically: outcome ~ TreatmentID + Covariate 1 + Covariate 2
        strPartOfFormula <- paste( c( "vTreatmentID", paste0( "lCovariates[[ ", seq_len( nQtyOfCovariates ), " ]]" ) ),
                                   collapse = " + " )
        strFormula       <- as.formula( paste( "lPatientOutcomes[[ i ]] ~", strPartOfFormula ) )

        # Perform ANCOVA
        dfAnalysisResult <- broom::tidy( aov( strFormula ) )

        # Extract the p values. Note that the last value is for residuals
        lPValues[[ i ] ] <- head( dfAnalysisResult$p.value, -1 )
        lDecision[[ i ] ] <- as.integer( lPValues [[ i ] ] <= dAlphaTwoSided )
    }

    # Return the analysis results, sample sizes of each group and means of outcomes
    lReturn <- list( Decision       = as.integer( 1 ),
                     ErrorCode      = as.integer( 0 ),
                     SampleSizeCtrl = as.integer( nSampleSizeCtrl ),
                     SampleSizeTrt  = as.integer( nSampleSizeTrt ) )

    # Add mean outcome and treatment decision
    for( i in 1:nQtyOfEndpoints )
    {
        lReturn[[ paste0( "MeanOutcome", i, "Ctrl" ) ] ]    <- as.double( vMeanOutcomeCtrl[ i ] )
        lReturn[[ paste0( "MeanOutcome", i, "Trt" ) ] ]     <- as.double( vMeanOutcomeTrt[ i ] )
        lReturn[[ paste0( "PValueOutcome", i , "Trt" ) ] ]  <- as.double( lPValues [[ i ] ][ 1 ] )
        lReturn[[ paste0( "DecisionOutcome", i, "Trt" ) ] ] <- as.integer( lDecision[[ i ] ][ 1 ] )

    }

    # Add p-values and covariate decisions
    for( j in 1:nQtyOfCovariates )
    {
        for( i in 1:nQtyOfEndpoints )
        {
            lReturn[[ paste0( "PValueOutcome", i, "Covariate", j ) ] ] <- as.double( lPValues[[ i ] ][ j + 1 ] )
            lReturn[[ paste0( "DecisionOutcome", i, "Covariate", j ) ] ] <- as.integer( lDecision[[ i ] ][ j + 1 ] )

        }
    }

    return( lReturn )

}
