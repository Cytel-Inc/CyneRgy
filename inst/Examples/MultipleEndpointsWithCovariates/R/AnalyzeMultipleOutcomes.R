######################################################################################################################## .
#' @name AnalyzeMultipleOutcomes
#' @title Analyze Simulated Patient Data with Multiple Independent Outcomes
#' 
#' @description This function performs endpoint-wise statistical analysis on simulated patient data. It compares 
#' treatment and control groups using a one-sided t-test assuming equal variances.
#' 
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. 
#'        It will have headers indicating the names of the columns. These names will be same as those used in 
#'        Data Generation. For analysis the most relevant variables are:
#'        \describe
#'        {
#'          \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1}
#'          \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2}
#'          \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3}
#'          \item{TreatmentID}{Integer vector (0 = control, 1 = treatment)}
#'        }          
#' @param DesignParam R List which consists of Design and Simulation Parameters which user may need to compute 
#'        test statistic and perform test. For analysis the most relevant variable is:
#'        \describe
#'        {
#'          \item{Alpha}{1-sided Type I Error}
#'        } 
#' @param LookInfo List. Not used in this function.
#' @param UserParam List. Not used in this function.     
#'        
#' @return A named list containing:
#'        \describe
#'        {
#'          \item{Decision}{Placeholder value (always 1)}
#'          \item{DecisionOutcome[X]}{Binary decisions (1 = significant, 0 = not significant) for endpoint X, where X = 1, 2, 3}
#'          \item{PValueOutcome[X]} {p-value for endpoint X, where X = 1, 2, 3}
#'          \item{SampleSizeCtrl}{Number of patients in the control group}
#'          \item{SampleSizeTrt}{Number of patients in the treatment group}
#'          \item{MeanOutcome[X]Ctrl}{Mean outcome for the control group for endpoint X, where X = 1, 2, 3}
#'          \item{MeanOutcome[X]Trt}{Mean outcome for the treatment group for endpoint X, where X = 1, 2, 3}
#'        }    
#' @examples
#' 
#' # Simulate patient data with three independent outcomes 
#' UserParam <- list(MeanOutcome1Ctrl = 10, MeanOutcome1Trt = 12,
#'                   MeanOutcome2Ctrl = 20, MeanOutcome2Trt = 22,
#'                   MeanOutcome3Ctrl = 30, MeanOutcome3Trt = 32)
#' 
#' NumSub      <- 100
#' TreatmentID <- rep(c(0,1), NumSub / 2, replace = TRUE)
#'                   
#' response <- SimulateMultipleOutcomes(NumSub = NumSub, 
#'                                      TreatmentID = TreatmentID,
#'                                      Mean = NULL, 
#'                                      StdDev = NULL, 
#'                                      UserParam = UserParam)
#'                                      
#' # Change the format of the simulated data (this is only required for testing the code in R, outside of East Horizon)                                               
#' response_df <- as.data.frame(response[c("PatientOutcome1", "PatientOutcome2", "PatientOutcome3")] )
#' response_df <- cbind(response_df, TreatmentID)
#' 
#' # Analyze the simulated patient data                                                           
#' result <- AnalyzeMultipleOutcomes(SimData = response_df,
#'                                   DesignParam = list("Alpha" = 0.025))                                                          
#'
#' @export

######################################################################################################################## .

AnalyzeMultipleOutcomes <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
  
  # Extract Type I error
  dAlpha <- DesignParam$Alpha

  # Create a vector of simulated data generated in 'Response' card
  vPatientOutcomes <- as.list( SimData[ , grep( "PatientOutcome", names( SimData ) ) ] )
  
  # Create a vector of treatment assignments
  vTreatmentID     <- SimData$TreatmentID
  
  # Determine the number of patients in each group
  nSampleSizeCtrl  <- sum( vTreatmentID == 0 )
  nSampleSizeTrt   <- sum( vTreatmentID == 1 )
  
  # Determine the number of endpoints
  nQtyOfEndpoints  <- length( vPatientOutcomes )
  
  # Initialize vectors to store results for each endpoint
  vPValue          <- rep( NA, nQtyOfEndpoints )
  vDecision        <- rep( NA, nQtyOfEndpoints )
  
  vMeanOutcomeCtrl <- rep( NA, nQtyOfEndpoints )
  vMeanOutcomeTrt  <- rep( NA, nQtyOfEndpoints )

  # Run t-test for each endpoint
  for ( i in 1:nQtyOfEndpoints )
  {
    vPatientOutcomeCtrl <- vPatientOutcomes[[ i ]][ vTreatmentID == 0 ]
    vPatientOutcomeTrt  <- vPatientOutcomes[[ i ]][ vTreatmentID == 1 ]
    
    vMeanOutcomeCtrl[ i ]  <- mean( vPatientOutcomeCtrl )
    vMeanOutcomeTrt [ i ]  <- mean( vPatientOutcomeTrt )

    lAnalysisResult <- t.test(  vPatientOutcomeTrt, vPatientOutcomeCtrl, alternative = "greater",
                               var.equal = TRUE )
    
    vPValue  [ i ]  <- lAnalysisResult$p.value 
    vDecision[ i ]  <- as.integer( vPValue [ i ] <= dAlpha )
  } 
  
  # Return the analysis results, sample sizes of each group and means of outcomes
  lReturn <- list( Decision         = as.integer( 1 ), 
                   DecisionOutcome1 = as.integer( vDecision[ 1 ] ),
                   DecisionOutcome2 = as.integer( vDecision[ 2 ] ),
                   DecisionOutcome3 = as.integer( vDecision[ 3 ] ),
                   PValueOutcome1   = as.double ( vPValue[ 1 ] ),
                   PValueOutcome2   = as.double ( vPValue[ 2 ] ),
                   PValueOutcome3   = as.double ( vPValue[ 3 ] ),
                   SampleSizeCtrl   = as.integer( nSampleSizeCtrl ),
                   SampleSizeTrt    = as.integer( nSampleSizeTrt ),
                   MeanOutcome1Ctrl = as.double( vMeanOutcomeCtrl[ 1 ] ),
                   MeanOutcome1Trt  = as.double( vMeanOutcomeTrt [ 1 ] ),
                   MeanOutcome2Ctrl = as.double( vMeanOutcomeCtrl[ 2 ] ),
                   MeanOutcome2Trt  = as.double( vMeanOutcomeTrt [ 2 ] ),
                   MeanOutcome3Ctrl = as.double( vMeanOutcomeCtrl[ 3 ] ),
                   MeanOutcome3Trt  = as.double( vMeanOutcomeTrt [ 3 ] ) )
  
  return( lReturn )
  
}