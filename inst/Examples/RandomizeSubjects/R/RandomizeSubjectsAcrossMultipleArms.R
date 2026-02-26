#' @name RandomizeSubjectsAcrossMultipleArms
#' @author Anoop Rawat
#' @title Randomize Subjects Across Multiple Arms
#' @description The following function randomly allots the subjects on one of the arms
#' @param NumSub: Mandatory. The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumArms: Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param AllocRatio: Mandatory. Vector containing the expected allocation ratios - relative to the control arm - for the treatment arms. Length of vector = (Number of arms - 1) 
#' @param UserParam : Optional. User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{TreatmentID}{Required value. This is a vector of treatment ID allocation per subject where:
#'                                  \describe{
#'                                    \item{TreatmentID = 0}{ Subject allotted to Control arm }
#'                                    \item{TreatmentID = n}{ Subject allotted to Experimental arm n where n >=1 }
#'                                    } 
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                      }
#'                      
RandomizeSubjectsAcrossMultipleArms <- function(NumSub, NumArms, AllocRatio, UserParam = NULL)
{
    
    Error                           <- 0
    
    # Allocation ratio on control and treatment arm
    vAllocRatio                     <- c( 1, AllocRatio ) # First arm (control) has ratio 1
    
    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arms
    vAllocFraction                  <- vAllocRatio / sum( vAllocRatio )
    
    # Calculate target sample sizes based on allocation ratio
    nTargetSampleSize               <- floor( NumSub * vAllocFraction)
    
    # Calculate how many subjects are left to allocate
    nRemaining                      <- NumSub - sum( nTargetSampleSize )
    
    # Allocate remaining subjects based on the fractional parts of the ideal allocation
    if (nRemaining > 0) {
        # Calculate fractional parts
        vFractionalParts            <- ( NumSub * vAllocFraction ) - nTargetSampleSize
        
        # Sort arms by fractional parts (descending) to prioritize allocation
        vArmOrder                   <- order( vFractionalParts, decreasing = TRUE )
        
        # Allocate remaining subjects to arms with highest fractional parts
        for ( i in 1:nRemaining ) {
            nTargetSampleSize[ vArmOrder[ i ]] <- nTargetSampleSize[ vArmOrder[ i ]] + 1
        }
    }
    
    # Create a vector with the treatment IDs (0 to NumArms-1)
    vAllTreatmentIDs <- 0:( NumArms - 1 )
    
    # Create a vector with the correct number of each treatment ID
    vTreatmentIDs    <- rep( vAllTreatmentIDs, times = nTargetSampleSize )
    
    # Randomly shuffle the treatment assignments
    vTreatmentIDs    <- sample( vTreatmentIDs )

    return( list( TreatmentID = as.integer( vTreatmentIDs ), ErrorCode = as.integer( Error )))
}