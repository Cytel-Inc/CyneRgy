######################################################################################################################## .
#' @name GenerateDropoutTimeMultiArmForSurvival
#' @title Generate Multi-Arm Time-to-Event Dropout Times
#' @description Generates subject-level dropout times for a multi-arm time-to-event trial from arm-specific hazard
#' rates or dropout probabilities.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param NumSub Integer. Number of subjects in the trial.
#' @param NumArm Integer. Number of arms in the trial, including control.
#' @param TreatmentID Integer vector of length `NumSub` containing arm indices, with 0 denoting control.
#' @param DropMethod Integer input method: 1 for dropout hazard rates or 2 for dropout probabilities.
#' @param NumPrd Integer. Number of dropout periods. This example uses one period.
#' @param PrdTime Numeric vector containing the times associated with `DropParam`.
#' @param DropParam Numeric matrix containing dropout parameters by period and arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @return A list containing `DropOutTime`, a numeric vector of length `NumSub` where `Inf` denotes no dropout, and
#' `ErrorCode`, an integer status code where 0 indicates success.
######################################################################################################################## .

GenerateDropoutTimeMultiArmForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime, DropParam, UserParam = NULL )
{
    nError <- 0

  # Initializing Censor Dropout Times to Inf
  # This effectively means that all the patients have dropped out at an infinite time,
  # i.e., effectively they haven't dropped out at all, meaning that they all are completers

  vDropoutTime <- rep( Inf, NumSub )

  if( DropMethod == 1 )    # Dropout Hazard Rates
  {
      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
      for( nArmIndex in seq( 0, NumArm - 1 ) )
      {
          if( DropParam[ nArmIndex + 1 ] > 0 )  # generate dropout time only in case of Non - zero dropout probability
          {
              # Identify the patients from various arms
              vIndexArm                     <- which( TreatmentID = nArmIndex )
              nQtyOfPatientonArm            <- length( vIndexArm )
              # Generate dropout time based on arm wise dropout parameters
              vDropoutTime[ vIndexArm ]     <- rexp( nQtyOfPatientonArm, rate = DropParam[ nArmIndex + 1 ] )
          }
      }
  }

  if( DropMethod == 2 )   # Probability of Dropout
  {
      # Conversion of dropout probabilities into Hazard rates
      dExpDropoutRate <- -log( 1 - DropParam ) / PrdTime

      # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.
      for( nArmIndex in seq( 0, NumArm - 1 ) )
      {
          if( DropParam[ nArmIndex + 1 ] > 0 )            # generate dropout time only in case of Non - zero dropout probability
          {
              # Identify the patients from various arms
              vIndexArm                     <- which( TreatmentID == nArmIndex )
              nQtyOfPatientonArm            <- length( vIndexArm )
              # Generate dropout time based on arm wise dropout parameters
              vDropoutTime[ vIndexArm ]     <- rexp( nQtyOfPatientonArm, rate = dExpDropoutRate[ nArmIndex + 1 ] )
          }
      }
  }

    return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError ) ) )
}
