#' @name GenerateDropoutTimeMultiArmForSurvival
#' @author Anoop Singh Rawat
#' @description The following function generates dropout time for a multi-arm survival design.
#' @param NumSub The number of patients or subjects that need to be simulated, integer value. 
#' @param NumArm The number of arms in the trial including experimental and control, integer value. 
#' @param TreatmentID Vector specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for placebo / control is 0. 
#' @param DropMethod Input method for specifying dropout parameters. 1 - Dropout Hazard rates and 2 - Probability of dropout.
#' @param NumPrd Number of dropout periods. In this example we fix NumPrd = 1
#' @param PrdTime Vector of times used to specify dropout parameters.
#' @param DropParam 2-D array of parameters used to generate dropout times. Number of rows = Number of Dropout Period . Number of Columns = Number of Arms including Control/Placebo.
#' In this example a Dropout Parameter will have only 1 row (Number of periods = 1)
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL. It is an optional parameter.
#' @return A list that contains: 
#' \describe{
#'     \item{ErrorCode (Optional)}{An integer value:  ErrorCode = 0 --> No Error
#'                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.}
#'     \item{DropOutTime (Mandatory)}{A numeric vector of length NumSub representing dropout times. Inf means no dropout. }

GenerateDropoutTimeMultiArmForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
{
  nError 	      <- 0
  
  # Initializing Censor Dropout Times to Inf
  # This effectively means that all the patients have dropped out at an infinite time, 
  # i.e., effectively they haven't dropped out at all, meaning that they all are completers 
  
  vDropoutTime <- rep( Inf, NumSub )
  
  if( DropMethod == 1 )    # Dropout Hazard Rates
  {
      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
      for( nArmIndex in seq( 0, NumArm - 1 ))
      {
          if( DropParam[ nArmIndex + 1 ] > 0 )  # generate dropout time only in case of Non - zero dropout probability
          {
              # Identify the patients from various arms
              vIndexArm                     <- which( TreatmentID = nArmIndex )
              nQtyOfPatientonArm            <- length( vIndexArm )
              # Generate dropout time based on arm wise dropout parameters
              vDropoutTime[ vIndexArm ]     <- rexp( nQtyOfPatientonArm, rate = DropParam[ nArmIndex + 1 ])
          }
      }
  }
  
  if( DropMethod == 2 )   # Probability of Dropout
  {
      # Conversion of dropout probabilities into Hazard rates
      dExpDropoutRate <- -log( 1 - DropParam ) / PrdTime
      
      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
      for( nArmIndex in seq( 0, NumArm - 1 ))
      {
          if( DropParam[ nArmIndex + 1 ] > 0 )            # generate dropout time only in case of Non - zero dropout probability
          {
              # Identify the patients from various arms
              vIndexArm                     <- which( TreatmentID == nArmIndex )
              nQtyOfPatientonArm            <- length( vIndexArm )
              # Generate dropout time based on arm wise dropout parameters
              vDropoutTime[ vIndexArm ]     <- rexp( nQtyOfPatientonArm, rate = dExpDropoutRate[ nArmIndex + 1 ])
          }
      }
  }
  
  return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError )));
}