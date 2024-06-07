
#'@name GenerateDropoutTimeForSurvival
#' @author Shubham Lahoti, J. Kyle Wathen
#' @description The following function generates dropout time for 2 arm survival design.
#' @param NumSub The number of patients or subjects that need to be simulated, integer value. 
#' @param NumArm The number of arms in the trial including experimental and control, integer value. 
#' @param TreatmentID Vector specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for placebo / control is 0. 
#' @param DropMethod Input method for specifying dropout parameters. 1 - Dropout Hazard rates and 2 - Probability of dropout.
#' @param NumPrd Number of dropout periods. In this example we fix NumPrd = 1
#' @param PrdTime Vector of times used to specify dropout parameters.
#' @param DropParam 2-D array of parameters used to generate dropout times. Number of rows = Number of Dropout Period . Number of Columns = Number of Arms including Control/Placebo.
#'  In this  example a Dropout Parameter will have only 1 row (Number of periods = 1) and 2 columns (one each for control and experimental arm)
#' @param UserParam A list of user defined parameters. The default must be NULL. It is an optional parameter.
#' @return A list that contains: 
#' \describe{
#'     \item{ErrorCode (Optional)}{An integer value:  ErrorCode = 0 --> No Error
#'                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.}
#'     \item{DropOutTime (Mandatory)}{A vector of length NumSub of censor indicator values with 0 for patients that dropout eg non-completer, 1 for no dropout, eg compelter. }


GenerateDropoutTimeForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
{
  
  Error 	      <- 0
  
  # Initializing Censor Dropout Times to Inf
  # This effectively means that all the patients have dropped out at an infinite time, 
  # i.e., effectively they haven't dropped out at all, meaning that they all are completers 
  
  vDropoutTime 	             <- rep( Inf, NumSub )
  
  #Identify the patients from Control and Experimental arm
  vIndexControl              <- which( TreatmentID == 0 )
  vIndexExperiment           <- which( TreatmentID == 1 )
  
  nQtyOfPatientOnControl     <- length( vIndexControl )
  nQtyOfPatientsOnExperiment <- length( vIndexExperiment)
  
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
      
      dExpDropoutControlRate      <-  -log(1 - DropParam[ 1 ]) / PrdTime
      dExpDropoutExperimentRate   <-  -log(1 - DropParam[ 2 ]) / PrdTime
      
      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
      
      if( DropParam[ 1 ] > 0 )            # generate dropout time only in case of Non - zero dropout probability
      {
        vDropoutTime[ vIndexControl ]   <- rexp( nQtyOfPatientOnControl, rate = dExpDropoutControlRate )
      }
      if( DropParam[ 2 ] > 0 )           # generate dropout time only in case of Non - zero dropout probability
      {
        vDropoutTime[ vIndexExperiment ]     <- rexp( nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate)
      }
    
  }
  
  return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( Error ) ) );
}