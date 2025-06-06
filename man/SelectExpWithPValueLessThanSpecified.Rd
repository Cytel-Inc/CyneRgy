% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectExpWithPValueLessThanSpecified.R
\name{SelectExpWithPValueLessThanSpecified}
\alias{SelectExpWithPValueLessThanSpecified}
\title{Select Experimental Treatments Using P-value Comparison}
\usage{
SelectExpWithPValueLessThanSpecified(
  SimData,
  DesignParam,
  LookInfo = NULL,
  UserParam = NULL
)
}
\arguments{
\item{SimData}{Dataframe containing data generated in the current simulation.}

\item{DesignParam}{List of design and simulation parameters required for treatment selection.}

\item{LookInfo}{List containing design and simulation parameters that might be required for treatment selection.}

\item{UserParam}{A list of user-defined parameters in East or East Horizon. Default is NULL.
The list must contain the following named element:
\describe{
\item{UserParam$dMaxPValue}{A value (0,1) specifying the chi-squared probability threshold for selecting treatments to advance.
Treatments with p-values less than this threshold will advance to the second stage.}
}}
}
\value{
A list containing:
\item{TreatmentID}{A vector of experimental treatment IDs selected to advance, e.g., 1, 2, ..., number of experimental treatments.}
\item{AllocRatio}{A vector of allocation ratios for the selected treatments relative to control.}
\item{ErrorCode}{An integer indicating success or error status:
\describe{
\item{ErrorCode = 0}{No error.}
\item{ErrorCode > 0}{Nonfatal error, current simulation aborted but subsequent simulations will run.}
\item{ErrorCode < 0}{Fatal error, no further simulations attempted.}
}
}
}
\description{
At the interim analysis, experimental treatments are compared to the control using a chi-squared test. Treatments with p-values
less than \code{dMaxPValue} are selected for stage 2. If no treatments meet the threshold, the treatment with the smallest p-value is selected.
In the second stage, the randomization ratio will be 1:1 (experimental:control).
}
\note{
\itemize{
\item The length of \code{TreatmentID} and \code{AllocRatio} must be the same.
\item The allocation ratio for control is always 1, and \code{AllocRatio} values are relative to this. For example, an allocation value of 2 means twice as many participants are randomized to the experimental treatment compared to control.
\item The order of \code{AllocRatio} should match \code{TreatmentID}, with corresponding elements assigned their respective allocation ratios.
\item The returned vector includes only \code{TreatmentID} values for experimental treatments. For example, \code{TreatmentID = c(0, 1, 2)} is invalid because control (\code{0}) should not be included.
\item At least one treatment and one allocation ratio must be returned.
}
}
\examples{
# Example 1: Allocation in the second stage is 1:2:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 1 and Experimental 2 both have an allocation ratio of 2.
vAllocationRatio    <- c(2, 2)
nErrorCode          <- 0
lReturn             <- list(TreatmentID = vSelectedTreatments, 
                             AllocRatio  = vAllocationRatio,
                             ErrorCode   = nErrorCode)
return(lReturn)

# Example 2: Allocation in the second stage is 1:1:2 for Control:Experimental 1:Experimental 2
vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many patients as Experimental 1 or Control.
vAllocationRatio    <- c(1, 2)
nErrorCode          <- 0
lReturn             <- list(TreatmentID = vSelectedTreatments, 
                             AllocRatio  = vAllocationRatio,
                             ErrorCode   = nErrorCode)
return(lReturn)
}
