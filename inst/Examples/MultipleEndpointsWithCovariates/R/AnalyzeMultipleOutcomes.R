######################################################################################################################## .
#' @name AnalyzeMultipleOutcomes
#' @title Analyze Simulated Patient Data with Multiple Independent Outcomes
#' @description This function performs endpoint-wise statistical analysis on simulated patient data. It compares
#'   treatment and control groups using a one-sided t-test assuming equal variances.
#' @author Julija Saltane
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#' }
#'
#' Additional fields returned by SimulateMultipleOutcomes:
#' \describe{
#'   \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1, with one element per subject.}
#'   \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2, with one element per subject.}
#'   \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3, with one element per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#' }
#' @param LookInfo Optional look information; not used by this example.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{DecisionOutcome[X]}{Binary decisions (1 = significant, 0 = not significant) for endpoint X, where X = 1,
#'     2, 3}
#'   \item{PValueOutcome[X]}{p-value for endpoint X, where X = 1, 2, 3}
#'   \item{SampleSizeCtrl}{Number of patients in the control group}
#'   \item{SampleSizeTrt}{Number of patients in the treatment group}
#'   \item{MeanOutcome[X]Ctrl}{Mean outcome for the control group for endpoint X, where X = 1, 2, 3.}
#'   \item{MeanOutcome[X]Trt}{Mean outcome for the treatment group for endpoint X, where X = 1, 2, 3.}
#' }
#'
#' Example-specific output usage: Decision: Placeholder value (always 1)
######################################################################################################################## .

AnalyzeMultipleOutcomes <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {

    # Extract Type I error
    dAlpha <- DesignParam$Alpha

    # Create a vector of simulated data generated in 'Response' card
    vPatientOutcomes <- as.list( SimData[ , grep( "PatientOutcome", names( SimData ) ) ] )

    # Create a vector of treatment assignments
    vTreatmentID <- SimData$TreatmentID

    # Determine the number of patients in each group
    nSampleSizeCtrl <- sum( vTreatmentID == 0 )
    nSampleSizeTrt <- sum( vTreatmentID == 1 )

    # Determine the number of endpoints
    nQtyOfEndpoints <- length( vPatientOutcomes )

    # Initialize vectors to store results for each endpoint
    vPValue <- rep( NA, nQtyOfEndpoints )
    vDecision <- rep( NA, nQtyOfEndpoints )

    vMeanOutcomeCtrl <- rep( NA, nQtyOfEndpoints )
    vMeanOutcomeTrt <- rep( NA, nQtyOfEndpoints )

    # Run t-test for each endpoint
    for ( i in 1:nQtyOfEndpoints ) {
        vPatientOutcomeCtrl <- vPatientOutcomes[[ i ]][ vTreatmentID == 0 ]
        vPatientOutcomeTrt <- vPatientOutcomes[[ i ]][ vTreatmentID == 1 ]

        vMeanOutcomeCtrl[ i ] <- mean( vPatientOutcomeCtrl )
        vMeanOutcomeTrt[ i ] <- mean( vPatientOutcomeTrt )

        lAnalysisResult <- stats::t.test( vPatientOutcomeTrt, vPatientOutcomeCtrl,
            alternative = "greater",
            var.equal = TRUE
        )

        vPValue[ i ] <- lAnalysisResult$p.value
        vDecision[ i ] <- as.integer( vPValue[ i ] <= dAlpha )
    }

    # Return the analysis results, sample sizes of each group and means of outcomes
    lReturn <- list(
        Decision = as.integer( 1 ),
        ErrorCode = as.integer( 0 ),
        DecisionOutcome1 = as.integer( vDecision[ 1 ] ),
        DecisionOutcome2 = as.integer( vDecision[ 2 ] ),
        DecisionOutcome3 = as.integer( vDecision[ 3 ] ),
        PValueOutcome1 = as.double( vPValue[ 1 ] ),
        PValueOutcome2 = as.double( vPValue[ 2 ] ),
        PValueOutcome3 = as.double( vPValue[ 3 ] ),
        SampleSizeCtrl = as.integer( nSampleSizeCtrl ),
        SampleSizeTrt = as.integer( nSampleSizeTrt ),
        MeanOutcome1Ctrl = as.double( vMeanOutcomeCtrl[ 1 ] ),
        MeanOutcome1Trt = as.double( vMeanOutcomeTrt[ 1 ] ),
        MeanOutcome2Ctrl = as.double( vMeanOutcomeCtrl[ 2 ] ),
        MeanOutcome2Trt = as.double( vMeanOutcomeTrt[ 2 ] ),
        MeanOutcome3Ctrl = as.double( vMeanOutcomeCtrl[ 3 ] ),
        MeanOutcome3Trt = as.double( vMeanOutcomeTrt[ 3 ] )
    )

    return( lReturn )
}
