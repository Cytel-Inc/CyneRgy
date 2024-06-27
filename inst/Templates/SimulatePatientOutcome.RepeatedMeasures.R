#'Last Modified Date: {{Date Created}}
#'@name {{FUNCTION_NAME}}
#'@param NumSub: Mandatory. The integer number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#'@param NumVisit: Mandatory. Integer number of Visits
#'@param TreatmentID: Mandatory. Array specifying indexes of arms to which subjects are allocated ﴾one arm index per subject. Index for placebo / control is 0.
#'@param Inputmethod: Mandatory. 0 - Actual values : Indicating that user has given mean and SD values for each visit. These are used to generate responses.
#'@param VisitTime: Mandatory. Numeric Visit Times
#'@param MeanControl: Mandatory. Numeric Control Mean for all visits
#'@param MeanTrt: Mandatory. Numeric Treatment Mean for all visits
#'@param StdDevControl: Mandatory. Numeric Control Standard Deviations for all visits
#'@param StdDevTrt: Mandatory. Numeric Treatment Standard Deviations for all visits
#'@param CorrMat: Mandatory. Correlation Matrix between all visits. Matrix of dimension n*n containing numeric values where n is number of visits. 
#'@param UserParam Optional. User can pass custom scalar variables defined by users as a member of this list. 
#'                  User should access the variables using names, for example UserParam$Var1 and not order. 
#'                  These variables can be of the following types: Integer, Numeric, or Character
#' 
#'@return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'                  \item{ErrorCode}{ Optional value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                                     
#'                  \item{Response<NumVisit>}{ A set of arrays of response for all subjects. Each array corresponds to each visit user has specified }             
#'                      
#'                      
{{FUNCTION_NAME}} <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    # TO DO : Modify this function appropriately
    Error 	    <- 0
    OutResponse <- c()
    retval      <- list()
    # Write the actual code here.
    # Store the generated continuous response values in # an array called retval.
    # Initializing Response Array to 0	
    for(i in 1:NumVisit)
    {
        strVisitName <- paste0( "Response", i )
        OutResponse <- rep( 0, NumSub )
        retval[[strVisitName]] <- as.double( OutResponse )
    }
    # Use appropriate error handling and modify the
    # Error appropriately 
    retval$ErrorCode <- as.integer( Error )
    return( retval )
}