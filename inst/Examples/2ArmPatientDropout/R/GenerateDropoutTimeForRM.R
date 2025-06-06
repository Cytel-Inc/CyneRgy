#' @name GenerateDropoutTimeForRM
#' @author Anusree Sengupta, Srinjoy Mondal
#' @description This function generates dropout time for a Repeated Measures design with Dropout method on East Horizon as 'Cumulative Probability of Dropout by Time'.
#' @param NumSub Mandatory. The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#' @param NumArm Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param TreatmentID Vector specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for placebo / control is 0. 
#' @param DropMethod Input method for specifying dropout parameters. 
#'           \describe{
#'           \item{Repeated Measures}{1 - Cumulative Probability of Dropout by Visit. 2 - Cumulative Probability of Dropout by Time}
#'           }
#' @param NumVisit Mandatory for Repeated Measures. Integer indicating number of visits.
#' @param VisitTime Mandatory for Repeated Measures. Vector containing numeric visit times for each visit.
#' @param ByTime Mandatory for Repeated Measures. Vector containing numeric by time for dropouts.
#' @param DropParamControl Mandatory for Repeated Measures. Vector containing numeric parameters used to generate dropout times for Control arm.
#' @param DropParamTrt Mandatory for Repeated Measures. Vector containing numeric parameters used to generate dropout times for Treatment arm.
#' @param UserParam : User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{DropOutTime}{ Applicable for Repeated measures. A numeric array of generated dropout times.}
#'                  \item{CensorInd[NumVisit]}{Applicable for Repeated Measures design. A set of arrays of censor indicator values for all subjects. Each array corresponds to each visit user has specified.}
#'                  \item{DropoutVisitID}{Applicable for Repeated Measures design. An array of 1-based Visit ID after which the patient dropped out.}
#'                      }
GenerateDropoutTimeForRM <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID, DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
{
    Error 	                   <- 0
    # Initializing Censor Dropout Times to Inf
    # This effectively means that all the patients have dropped out at an infinite time, 
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers
    # We modify this vector later
    vDropoutTime 	           <- rep( Inf, NumSub )
    
    #Identify the patients from Control and Experimental arm
    vIndexControl              <- which( TreatmentID == 0 )
    vIndexExperiment           <- which( TreatmentID == 1 )
    
    nQtyOfPatientOnControl     <- length( vIndexControl )
    nQtyOfPatientsOnExperiment <- length( vIndexExperiment)
    
    if( DropMethod == 2 )    # Cumulative Probability of Dropout by Time 
    {
        # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
        
        if( DropParamControl > 0 )  # generate dropout time only in case of Non - zero dropout probability
        {
            dExpDropoutControlRate           <-  -log(1 - DropParamControl) / ByTime
            
            vDropoutTime[ vIndexControl ]    <- rexp( nQtyOfPatientOnControl, rate = dExpDropoutControlRate )
        }
        if( DropParamTrt > 0 )             # generate dropout time only in case of Non - zero dropout probability
        {
            dExpDropoutExperimentRate        <-  -log(1 - DropParamTrt) / ByTime
            
            vDropoutTime[ vIndexExperiment ] <- rexp( nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate)
        }
    }	
    
    # Repeated Measures Dropout Output Hierarchy
    # Step 1: If user has returned Censor Indicator arrays CensorInd1, CensorInd2, ..., CensorInd[NumVisit] from their R code, 
    # then no other outputs are required. In that case, all other outputs become optional and the workflow ends here. 
    # If user has not returned Censor Indicator arrays from their R code, please go to Step 2.
    # 
    # Step 2: If user has returned DropoutVisitID from their R code, then no other outputs are required. 
    # In that case, all other outputs become optional and the workflow ends here.  
    # If user has not returned DropoutVisitID from their R code, please go to Step 3.
    # 
    # Step 3: If user has returned DropOutTime from their R code, then simulations run successfully and 
    # all other outputs becomes optional. no other outputs are required. If user has not returned DropOutTime from their R code, 
    # then the application will return an error code. The workflow ends here.
    
    return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( Error ) ) );
}
