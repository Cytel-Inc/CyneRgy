#' @name SimulatePatientSurvivalAssuranceUsingPh2Prior
#' @title Simulate Patient Survival Times Using a Phase 2 Prior for Assurance
#' @description Function simulates from exponential, just included as a simple example as a starting point
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2, length( TreatmentID ) = NumSub
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided. 
#' @param PrdTime \describe{ 
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative % survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default}
#'      }
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param  UserParam A list of user defined parameters in East or East Horizon. The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dIntercept}{Intercept for the linear relationship between true treatment difference and log(HR).}
#'      \item{UserParam$dSlope}{Slope for the linear relationship between true treatment difference and log(HR).}
#'      \item{UserParam$dMeanTTECtrl}{Mean time-to-event for the control group.}
#'   }

SimulatePatientSurvivalAssuranceUsingPh2Prior <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
      if( !exists( "gvPrior" ) )
      {
        # Load prior obtained from Phase 2
        gvPrior     <<- LoadData()
        gnIndex    <<- 1
      }
    
    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    vTrtAllocation <- table( TreatmentID )
    vSurvTime      <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    ErrorCode    <- rep( -1, NumSub ) 
    
    # Step 2: Using the true treatment difference from Ph 2, compute the log( true hazard ratio) ####
    dTrueTreatmentDiff <- gvPrior[ gnIndex ]
    gnIndex <<- gnIndex + 1
    
    dLogTrueHazardRatio <- UserParam$dIntercept + UserParam$dSlope * dTrueTreatmentDiff
    dTrueHazardRatio    <- exp( dLogTrueHazardRatio )
   
    # Step 3: Compute the hazard on experimental given the true hazard on control and the computed true hazard ratio ####
    
    dRateCtrl        <- 1.0/UserParam$dMeanTTECtrl 
    dRateExp         <- dTrueHazardRatio * dRateCtrl
    
    vRates      <- c( dRateCtrl, dRateExp )
    
    vTrt1 <- rexp( vTrtAllocation[ 1 ], vRates[ 1 ] )
    vTrt2 <- rexp( vTrtAllocation[ 2 ], vRates[ 2 ] )
    
    vSurvTime[ TreatmentID == 0 ] <- vTrt1
    vSurvTime[ TreatmentID == 1 ] <- vTrt2
       
    
    return( list( SurvivalTime = as.double( vSurvTime ), TrueHR = as.double( rep( dTrueHazardRatio, NumSub ) ), ErrorCode = ErrorCode ) )
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
    select( dTrueDelta )
  
  return( dfConditionalPostOnPh2Success$dTrueDelta )
}