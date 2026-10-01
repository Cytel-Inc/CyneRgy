######################################################################################################################## .
#' @name SimulatePatientSurvivalAssuranceUsingPh2Prior
#'
#' @title Simulate Patient Survival Times Using a Phase 2 Prior for Assurance
#'
#' @description Generate exponential survival times using successive treatment effects from successful Phase 2
#'   simulations. Transform each effect to a log hazard ratio using UserParam$dIntercept and UserParam$dSlope,
#'   then use the control mean time-to-event and that hazard ratio to determine the arm-specific hazard rates.
#'
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#'
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#'
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification).
#'
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' UserParam must be supplied and must contain the following named elements:
#' \describe{
#'      \item{UserParam$dIntercept}{Intercept for the linear relationship between true treatment difference and
#'        log(HR).}
#'      \item{UserParam$dSlope}{Slope for the linear relationship between true treatment difference and log(HR).}
#'   \item{UserParam$dMeanTTECtrl}{Positive numeric mean time-to-event for the control arm.}
#'   }
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{TrueHR}{Numeric vector of sampled treatment-to-control hazard ratios, with one element per subject.
#'     The same hazard ratio is repeated for all subjects in a simulation.}
#' }

######################################################################################################################## .

SimulatePatientSurvivalAssuranceUsingPh2Prior <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    if ( !exists( "gvPrior" ) ) {
        # Load prior obtained from Phase 2
        gvPrior <<- LoadData( )
        gnIndex <<- 1
    }

    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    nControlSubjects <- sum( TreatmentID == 0 )
    nExperimentalSubjects <- sum( TreatmentID == 1 )
    vSurvTime <- rep( -1, NumSub ) # The vector of patient survival times that will be returned.

    nErrorCode <- 0L

    if ( !exists( "gnIndex" ) || gnIndex < 1 || gnIndex > length( gvPrior ) ) {
        return( list( SurvivalTime = rep( 0, NumSub ), ErrorCode = -100L ) )
    }

    # Step 2: Using the true treatment difference from Ph 2, compute the log( true hazard ratio) ####
    dTrueTreatmentDiff <- gvPrior[ gnIndex ]
    gnIndex <<- gnIndex + 1

    dLogTrueHazardRatio <- UserParam$dIntercept + UserParam$dSlope * dTrueTreatmentDiff
    dTrueHazardRatio <- exp( dLogTrueHazardRatio )

    # Step 3: Compute the hazard on experimental given the true hazard on control and the computed true hazard ratio ####

    dRateCtrl <- 1.0 / UserParam$dMeanTTECtrl
    dRateExp <- dTrueHazardRatio * dRateCtrl

    vRates <- c( dRateCtrl, dRateExp )

    vTrt1 <- stats::rexp( nControlSubjects, vRates[ 1 ] )
    vTrt2 <- stats::rexp( nExperimentalSubjects, vRates[ 2 ] )

    vSurvTime[ TreatmentID == 0 ] <- vTrt1
    vSurvTime[ TreatmentID == 1 ] <- vTrt2

    if ( any( !is.finite( vSurvTime ) ) ) {
        nErrorCode <- -100L
    }

    return( list( SurvivalTime = as.double( vSurvTime ), TrueHR = as.double( rep( dTrueHazardRatio, NumSub ) ), ErrorCode = nErrorCode ) )
}

LoadData <- function( ) {
    # Step 1 - Process the East Horizon Explore results for Phase 2
    # The CSV file will contain 1 row for each IA that was conducted and the FA.  However, if the trial is stopped early for efficacy or futility
    # it will only contain the IAs that occur.  Therefore, we must find the last analysis for each simulated trial.
    dfEastHorExp <- readr::read_csv( "Inputs/Ph2_results.csv" )

    # Build a data frame with only 1 row per simulated trial with the last analysis.  The last analysis is the analysis (IA or FA) that makes a futility or efficacy decision.
    dfLastAnalysisResults <- dplyr::ungroup(
        dplyr::slice_max(
            dplyr::group_by( dfEastHorExp, SimIndex ),
            AnalysisIndex
        )
    )

    # Step 2 - The Ph3 is only conducted when the Ph2 is successful (Efficacy) so create a data frame of the simulated trials that are successful
    # Select trials that are successful so we can build posterior of true delta when a Go decision is made
    dfConditionalPostOnPh2Success <- dplyr::select(
        dfLastAnalysisResults[ dfLastAnalysisResults$Decision == "Efficacy", ],
        dTrueDelta
    )

    return( dfConditionalPostOnPh2Success$dTrueDelta )
}
