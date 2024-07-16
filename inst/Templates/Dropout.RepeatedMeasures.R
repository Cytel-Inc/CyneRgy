#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
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
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{DropOutTime}{ Applicable for Repeated measures. A numeric array of generated dropout times.}
#'                  \item{CensorInd<NumVisit>}{Applicable for Repeated Measures design. A set of arrays of censor indicator values for all subjects. Each array corresponds to each visit user has specified.}
#'                  \item{DropoutVisitID}{Applicable for Repeated Measures design. An array of 1-based Visit ID after which the patient dropped out}
#'                      }


{{FUNCTION_NAME}} <- function(NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                    DropMethod, ByTime, DropParamControl, DropParamtrt, UserParam = NULL)
{
  # TO DO : Modify this function appropriately 
    Error     <-  0
    initval   <- c()
    retval    <- list()
    
    # Initializing CensorInd Arrays
    for(i in 1:NumVisit)
    {
        strCensorIndName <- paste0( "CensorInd", i )
        CensorInd        <- rep(1,NumSub)
        retval[[strCensorIndName]] <- as.integer(CensorInd)
    }
  
    # Initializing DropOutTime and DropoutVisitID to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers
    for(i in 1:NumSub)
    {
        initval[i] = Inf;
    }
  
    retval$DropoutVisitID <- as.integer(initval)
    retval$DropOutTime    <- as.double(rep(NumVisit, NumSub))
  
    # Use appropriate error handling and modify the
    # Error appropriately in each of the methods
    retval$ErrorCode <- as.integer(Error)
    
    # Repeated Measures Dropout Output Hierarchy
    # Step 1: If user has returned Censor Indicator arrays CensorInd1, CensorInd . CensorInd<NumVisit> from their R code, 
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
    
    
    #retval is one of the options: 1) CensorID, 2) VisitID, 3) DropOutTime
    
    return( retval );
}
