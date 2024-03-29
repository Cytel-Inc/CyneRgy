#' @param SimulatePatientOutcomeNormalAssurance
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dWeight1}{Probability of sampiling from part 1} 
#'      \item{UserParam$dWeight}{Probability of sampling from part 2} 
#'      \item{UserParam$dMean1}{Prior mean for part 1} 
#'      \item{UserParam$dMean2}{Prior mean for part 2}
#'      \item{UserParam$dSD1}{Prior SD for part 1}
#'      \item{UserParam$dSD2}{Prior SD for part 2}
#'      \item{UserParam$dWeight1}{Weight of prior 1}
#'      \item{UserParam$dWeight2}{Weight of prior 2}
#'      \item{UserParam$dMeanCtrl}{Mean form control } 
#'  }
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
SimulatePatientOutcomeNormalAssurance <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    
    # Note: Example of how you could save the parameters in East. Do NOT setwd in Solara
     #setwd( "C:/AssuranceNormal/ExampleArgumentsFromEast/Example2")
    # #if( !file.exists("SimData.Rds"))
    # #{
        # saveRDS( NumSub,     "NumSub.Rds")
        # saveRDS( TreatmentID, "TreatmentID.Rds" )
        # saveRDS( StdDev, "StdDev.Rds" )
        # saveRDS( UserParam,   "UserParam.Rds")
    # #}
    
    # Step 1 - Setup the vectors so we can sample which component of the mixture prior to use
    vStdDev     <- c( UserParam$dSDCtrl, UserParam$dSDExp )
    vMean       <- c( UserParam$dMeanCtrl )                     #Note: only need control mean as we will sample experimental mean
    vPriorMeans <- c( UserParam$dMean1, UserParam$dMean2 )
    vPriorSDs   <- c( UserParam$dSD1, UserParam$dSD2 )
    
    # Step 2 - Sample the prior mean treatment effect according to the weights of the two normal distributions in the mixture 
    nPrior           <- sample( c(1, 2 ), 1 , prob = c( UserParam$dWeight1, UserParam$dWeight2 ), replace = TRUE )
    dTreatmentEffect <- rnorm( 1, vPriorMeans[ nPrior ], vPriorSDs[ nPrior ] )
    vMean            <- c( vMean,  vMean[ 1 ] + dTreatmentEffect)
    
    # Step 3 - Initialize variable ####   
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Make any adjustments to the code as needed, example simulating from for a normal distribution 
        vPatientOutcome[ nPatIndx ] <- rnorm( 1, vMean[ nTreatmentID ], vStdDev[ nTreatmentID ] )
    }
    
    # Step 5 - Error Checking ####
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    # Step 6 - Create any variables that are returned that need to be included in the output
    # Note: Need to return the true delta, and East expects it to be a vector.  
    TrueDelta <- rep( vMean[2], length( vPatientOutcome))
    
    # Step 7 - Build the return object, add other variables to the list as needed
    #       Add the vTrueDeta so it can easily be output by saving the East summary stats. 
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ), vTrueDelta = as.double(TrueDelta), Delta = as.double( TrueDelta)  )
    
    return( lReturn )
}


#' @param SimulatePatientOutcomeNormalAssuranceUsingPriorInput
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   The default must be NULL resulting in ignoring the percent of patients at 0.
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
SimulatePatientOutcomeNormalAssuranceUsingPriorInput <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # Step 1 - Use the prior that was supplied from output ####
    if( exists("vPrior"))
        nError <- -100
    
    Mean[ 2 ]   <- vPrior[ nSimIndex ]
    nSimIndex   <<- nSimIndex + 1 # remove the first element so the next call gets a different true delta
    
    # Step 2 - Setup the vectors so we can sample which component of the mixture prior to use ####
    vStdDev     <- c( UserParam$dSDCtrl, UserParam$dSDExp )
    
    
    # Step 3 - Initialize variable ####   
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Make any adjustments to the code as needed, example simulating from for a normal distribution 
        vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ],vStdDev[ nTreatmentID ] )
    }
    
    # Step 5 - Error Checking ####
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    # Step 6 - Create any variables that are returned that need to be included in the output
    TrueDelta <- rep( Mean[2], length( vPatientOutcome))
    
    # Step 7 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ), vTrueDelta = as.double(TrueDelta), Delta = as.double( TrueDelta)  )
    
    return( lReturn )
}

