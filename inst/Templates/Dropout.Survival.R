######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Generate Time-to-Event Dropout Times
#' @description Generate subject-level dropout times from arm-specific hazard rates or dropout probabilities.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param DropMethod Integer input method: 1 for dropout hazard rates or 2 for cumulative dropout probabilities.
#' @param NumPrd Integer number of dropout periods. Mandatory for time-to-event endpoints.
#' @param PrdTime Numeric vector containing the start time of each dropout period.
#' @param DropParam Numeric matrix with `NumPrd` rows and `NumArm` columns. For `DropMethod = 1`, entries are dropout hazard rates by period and arm; for `DropMethod = 2`, entries are cumulative dropout probabilities by period and arm.
#'           \describe{
#'           \item{Number of rows = Number of Dropout periods.}
#'           \item{Number of columns = Number of arms including control/placebo.}
#'           }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                    User should access the variables using names, for example UserParam$Var1 and not order.
#'                    These variables can be of the following types: Integer, Numeric, or Character
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{DropOutTime}{ Mandatory
#'                  \describe{ A numeric array of generated dropout times }
#'                      }
#'                }
#'
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
{

    nError <- 0

    # Initializing Censor Dropout Times to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers

    vDropoutTime <- rep( Inf, NumSub )

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
