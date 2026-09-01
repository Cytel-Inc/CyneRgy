######################################################################################################################## .
#' @name GenerateDropoutTimeForSurvival
#' @title Generate Dropout Times for a Survival Design
#' @author Shubham Lahoti
#' @description The following function generates dropout time for 2-arm survival design.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param DropMethod Integer input method: 1 for dropout hazard rates or 2 for cumulative dropout probabilities.
#' @param NumPrd Integer number of dropout periods. This example uses one period.
#' @param PrdTime Numeric vector containing the start time of each dropout period.
#' @param DropParam Numeric matrix with `NumPrd` rows and `NumArm` columns. For `DropMethod = 1`, entries are dropout hazard rates by period and arm; for `DropMethod = 2`, entries are cumulative dropout probabilities by period and arm.
#'  In this  example a Dropout Parameter will have only 1 row (Number of periods = 1) and 2 columns (one each for control and experimental arm)
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @return A list that contains:
#' \describe{
#'     \item{ErrorCode (Optional)}{An integer value:  ErrorCode = 0 --> No Error
#'                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.}
#'     \item{DropOutTime (Mandatory)}{A numeric vector of length NumSub representing dropout times. Inf means no dropout. }
#' }
######################################################################################################################## .
GenerateDropoutTimeForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
{

  nError          <- 0

  # Initializing Censor Dropout Times to Inf
  # This effectively means that all the patients have dropped out at an infinite time,
  # i.e., effectively they haven't dropped out at all, meaning that they all are completers

  vDropoutTime                  <- rep( Inf, NumSub )

  #Identify the patients from Control and Experimental arm
  vIndexControl              <- which( TreatmentID == 0 )
  vIndexExperiment           <- which( TreatmentID == 1 )

  nQtyOfPatientOnControl     <- length( vIndexControl )
  nQtyOfPatientsOnExperiment <- length( vIndexExperiment )

  if( DropMethod == 1 )    # Dropout Hazard Rates
  {
      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.

      if( DropParam[ 1 ] > 0 )  # generate dropout time only in case of Non - zero dropout probability
      {
          vDropoutTime[ vIndexControl ] <- rexp( nQtyOfPatientOnControl, rate = DropParam[ 1 ] )
      }
      if( DropParam[ 2 ] > 0 )             # generate dropout time only in case of Non - zero dropout probability
      {
          vDropoutTime[ vIndexExperiment ] <- rexp( nQtyOfPatientsOnExperiment, rate = DropParam[ 2 ] )
      }
  }

  if( DropMethod == 2 )   # Probability of Dropout
  {
      # Conversion of dropout probabilities into Hazard rates

      dExpDropoutControlRate      <-  -log( 1 - DropParam[ 1 ] ) / PrdTime
      dExpDropoutExperimentRate   <-  -log( 1 - DropParam[ 2 ] ) / PrdTime

      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.

      if( DropParam[ 1 ] > 0 )            # generate dropout time only in case of Non - zero dropout probability
      {
        vDropoutTime[ vIndexControl ]   <- rexp( nQtyOfPatientOnControl, rate = dExpDropoutControlRate )
      }
      if( DropParam[ 2 ] > 0 )           # generate dropout time only in case of Non - zero dropout probability
      {
        vDropoutTime[ vIndexExperiment ]     <- rexp( nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate )
      }

  }

  return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError ) ) )
}
