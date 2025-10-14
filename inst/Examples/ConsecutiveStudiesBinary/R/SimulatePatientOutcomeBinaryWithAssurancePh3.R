######################################################################################################################## .
#' @name SimulatePatientOutcomeBinaryWithAssurancePh3
#' @title Simulate binary patient outcomes using Phase 2 posterior distribution
#' 
#' @description Generate patient outcomes for a binary response trial while incorporating uncertainty about the true
#' response rates by sampling them from the posterior distribution obtained from Phase 2.
#' 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial including experimental and control, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param PropResp A vector of expected proportions of response for each arm
#' @param UserParam A list of user defined parameters in East Horizon. The UserParam must be NULL or is ignored in this R script
######################################################################################################################## .

SimulatePatientOutcomeBinaryWithAssurancePh3 <- function( NumSub, NumArm, TreatmentID, PropResp, UserParam = NULL )
{
  # Step 1: Sample true probability of response 
  if( !exists( "gdfPh2Post" ) )
  {
    # Load posterior distribution obtained from Phase 2
    gdfPh2Post <<- LoadData()
    gnIndex    <<- 1
  }
  
  dTrueProbCtrl <- gdfPh2Post$TrueProbabilityControl[ gnIndex ]
  dTrueProbExp  <- gdfPh2Post$TrueProbabilityExperimental[ gnIndex ]
  gnIndex       <<- gnIndex + 1
  vTrueProb     <- c( dTrueProbCtrl, dTrueProbExp )
  
  
  nError           <- 0 # Code for no errors occurred 
  vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
  
  
  # Loop over the patients and simulate the outcome according to the treatment they 
  for( nPatIndx in 1:NumSub )
  {
    nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
    vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, vTrueProb[ nTreatmentID ] )
  }
  
  if(  any( is.na( vPatientOutcome ) == TRUE) )
    nError <- -100
  
  # True Probability of Responses have to be a vector of same length to number of subjects 
  
  lReturn <- list( Response = as.double( vPatientOutcome ), 
                   ErrorCode = as.integer( nError ),  
                   nIndex    = as.integer( gnIndex ),
                   TrueProbabilityControl = as.double( rep( dTrueProbCtrl, NumSub ) ), 
                   TrueProbabilityExperimental = as.double( rep(dTrueProbExp, NumSub ) ) )
  return( lReturn )
}

LoadData <- function()
{
  library(dplyr)
  # Step 1 - Process the East Horizon Explore results for Phase 2
  # The CSV file will contain 1 row for each IA that was conducted and the FA.  However, if the trial is stopped early for efficacy or futility
  # it will only contain the IAs that occur.  Therefore, we must find the last analysis for each simulated trial. 
  dfEastHorExp <- readr::read_csv( "Inputs/Ph2_results.csv" )
  
  # Build a dataframe with only 1 row per simulated trial with the last analysis.  The last analysis is the analysis (IA or FA) that makes a futility or efficacy decision.
  dfLastAnalysisResults <-  group_by( dfEastHorExp, SimIndex ) %>%
    slice_max( AnalysisIndex ) %>%
    ungroup()
  
  # Step 2 - The Ph3 is only conducted when the Ph2 is successful (Efficacy) so create a dataframe of the simulated trials that are successful
  # Select trials that are successful so we can build posterior of true delta when a Go decision is made
  dfConditionalPostOnPh2Success <- dfLastAnalysisResults[ dfLastAnalysisResults$Decision == "Efficacy", ] %>% 
    select( TrueProbabilityControl, TrueProbabilityExperimental )
  
  return( dfConditionalPostOnPh2Success )
}
