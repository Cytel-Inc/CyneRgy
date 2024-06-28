#  Last Modified Date: {{27th June 2024}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub Mandatory. The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#' @param NumArm Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param TreatmentID Vector specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for placebo / control is 0. Mandatory for Time To Event and Repeated measures designs.
#' @param DropMethod Input method for specifying dropout parameters. Mandatory for Time to Event and Repeated measures designs.
#'           \describe{
#'           \item{Time to Event}{2 - Probability of dropouts}
#'            }
#' @param NumPrd The integer value specifying number of dropout periods. Mandatory for Time to Event endpoint
#' @param PrdTime Vector of numeric time values used to specify dropout parameters. Mandatory for Time to Event endpoint
#' @param DropParam A 2D array of parameters used to generate dropout times. Mandatory for Time to Event endpoint
#'           \describe{
#'           \item{Number of rows = Number of Dropout periods.}
#'           \item{Number of columns = Number of arms including control/placebo.}
#'           }
#' @param UserParam : User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{DropOutTime}{ Mandatory
#'                  \describe{ A numeric array of generated dropout times }
#'                      }
#'                }
#'                      

dropout <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
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