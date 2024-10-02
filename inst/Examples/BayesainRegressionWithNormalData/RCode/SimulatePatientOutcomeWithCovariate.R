#' @name SimulatePatientOutcomeWithCovariate
#' @title Simulate patient outcome using a covariate for patient prognosis.  
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dCtrlBetaParam1}{First parameter in the Beta distribution for the control (ctrl) treatment.}
#'      \item{UserParam$dCtrlBetaParam2}{Second parameter in the Beta distribution for the control (ctrl) treatment.}
#'      \item{UserParam$dExpBetaParam1}{First parameter in the Beta distribution for the experimental (exp) treatment}.
#'      \item{UserParam$dExpBetaParam2}{Second parameter in the Beta distribution for the experimental (exp) treatment.  }
#' }
#'   
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#'             
#' @description
#' The function assumes that the probability a patient has a zero response is random and follows a Beta( a, b ) distribution.
#' Each distribution must provide 2 parameters for the beta distribution and the probability of 0 outcome is selected from the corresponding Beta distribution.
#' The probability of 0 outcome on the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 ) distribution.
#' The probability of 0 outcome on the experimental treatment is sampled from a Beta( UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution.
#' The intent of this option is to incorporate the variability in the unknown variable, probability of no response.  
SimulatePatientOutcomeWithCovariate <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    
    # Mean[ nTreatmentID ] + dBeta*Z*( Mean[ nTreatmentID ])
    # Must specify the Pr( Z = 1) as dProbOfGoodPrognosis

    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        dProbOfGoodPrognosis <- 0
        dBeta2               <- 0
        dBeta3               <- 0
        vGoodPrognosis       <- rep( 0, NumSub )   # No patients are Good Prognosis by default, eg this ignores the covariate 
    }
    else
    {
        # Simulate the patient prognosis from a binomial distribution
        dProbOfGoodPrognosis <- UserParam$dProbOfGoodPrognosis
        dBeta2               <- UserParam$dBeta2 
        dBeta3               <- UserParam$dBeta3 
        vGoodPrognosis       <- rbinom( NumSub, 1, dProbOfGoodPrognosis )
    }
    
    dBeta0           <- Mean[ 1 ]
    dBeta1           <- Mean[ 2 ] - Mean[ 1 ]
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index

        # From the regression model, the mean is Beta0 + Beta1*TreatmentID + Beta2 * I(Good Prognosis) + Beta3 * I( Good Prognosis ) * TreatmentID
        dMean                       <- dBeta0 + dBeta1*TreatmentID[ nPatIndx ] + dBeta2 * vGoodPrognosis[ nPatIndx ] + dBeta3 * vGoodPrognosis[ nPatIndx ] * TreatmentID[ nPatIndx ] 
        vPatientOutcome[ nPatIndx ] <- rnorm( 1, dMean, StdDev[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ), GoodPrognosis = as.integer( vGoodPrognosis ) )
 
    return( lReturn )
}
