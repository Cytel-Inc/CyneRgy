#'Last Modified Date: {{27th June 2024}}
#'@name {{FUNCTION_NAME}}
#'@param NumSub: The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#'@param ProbDrop: A Dropout probability for both the arms. The argument value is passed from Engine.
#'@param NumVisit: Number of Visits
#'@param TreatmentID: Array specifying indexes of arms to which subjects are allocated ﴾one arm index per subject. Index for placebo / control is 0.
#'@param Inputmethod: There were two options  1) Actual values , 2) Change from baseline. 
#'Actual values: You give mean and SD values for each visit and using those you will generate responses.
#'Change from baseline: Expected change from baseline at each visit rather than the true means.
#'@param VisitTime: Visit Times
#'@param MeanControl: Control Mean for all visits
#'@param MeanTrt: Treatment Mean for all visits
#'@param StdDevControl: Control Standard Deviations for all visits
#'@param StdDevTrt: Treatment Standard Deviations for all visits
#'@param CorrMat: Correlation Matrix between all visits
#'@param UserParam User can pass custom scalar variables defined by users as a member of this list. 
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
#'                  \item{Response}{ A set of arrays of response for all subjects. Each array corresponds to each visit user has specified }             
#'                                     
#'                      }
#'                      
#'                      
{{FUNCTION_NAME}} <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
  nError                         <- 0
  vResponse                      <- list()
  
  # Step 1 - If LookInfo is Null, then this is a fixed design and we use the DesignParam$MaxEvents
  
  nLookIndex           <- 1 
  if(  !is.null( LookInfo )  )
  {
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
  }
  else
  {
    nQtyOfLooks          <- 1
    nQtyOfPatsInAnalysis <- nrow( SimData )
  }
  
  return(list(Response = as.double(vResponse), ErrorCode = as.integer(nError)))
}