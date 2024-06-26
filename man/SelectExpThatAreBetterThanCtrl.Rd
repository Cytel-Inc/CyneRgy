% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectExpThatAreBetterThanCtrl.R
\name{SelectExpThatAreBetterThanCtrl}
\alias{SelectExpThatAreBetterThanCtrl}
\title{Select treatments that are higher than control or, if none are greater, select the treatment with the largest probability of response.}
\usage{
SelectExpThatAreBetterThanCtrl(
  SimData,
  DesignParam,
  LookInfo,
  UserParam = NULL
)
}
\arguments{
\item{SimData}{Data frame which consists of data generated in current simulation.}

\item{DesignParam}{List of Design and Simulation Parameters required to perform treatment selection.}

\item{LookInfo}{List containing Design and Simulation Parameters, which might be required to perform treatment selection}

\item{UserParam}{A list of user defined parameters in East. The default must be NULL.}
}
\value{
TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments

AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.

ErrorCode An integer value:  ErrorCode = 0 --> No Error
}
\description{
At the interim analysis, select any treatment with a response rate that is higher than control for stage 2.
 If none of the treatments have a higher response rate than control, select the treatment with the largest probability of response.
 In the second stage, the randomization ratio will be 1:1 (experimental:control).
}
\note{
The length of TreatmentID and AllocRatio must be the same.

The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental

The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio

The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.

You must return at LEAST one treatment and one allocation ratio

Helpful Hints:
      There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
      objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.

      saveRDS( SimData,     "SimData.Rds")
      
      saveRDS( DesignParam, "DesignParam.Rds" )
      
      saveRDS( LookInfo,    "LookInfo.Rds" )

      The above code will save each of the input objects to a file so they may be examined within R.
}
\examples{
 
      # Example Output Object:
      #Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
      vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2. 
      vAllocationRatio    <- c( 2, 2 )
      nErrorCode          <- 0
      lReturn             <- list( TreatmentID = vSelectedTreatments, 
                                   AllocRatio  = vAllocationRatio,
                                   ErrorCode   = nErrorCode )
      return( lReturn )
      
      #Example 2: Assuming the allocation in 2nd part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
      vSelectedTreatments <- c( 1, 2 )  # Experimental 2 will receive twice as many as Experimental 1 or Control. 
      vAllocationRatio    <- c( 1, 2 )
      nErrorCode          <- 0
      lReturn             <- list( TreatmentID = vSelectedTreatments, 
                                   AllocRatio  = vAllocationRatio,
                                   ErrorCode   = nErrorCode )
      return( lReturn )

}
