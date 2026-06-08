# Last Modified Date: {{CREATION_DATE}}

#' @name {{FUNCTION_NAME}}
#' @title Template for analyzing survival outcomes using the log-rank test with Bonferroni adjustment.
#'
#' @param SimData A data frame containing simulated patient level data.
#'        Required variables in the data frame include:
#'        \describe{
#'        \item{ArrivalTime}{Patient enrollment time, numeric vector}
#'        \item{TreatmentID}{Treatment assignment where 0 = control and 1,2,... represent treatment arms}
#'        \item{SurvivalTime}{Observed or simulated survival time for each patient}
#'        }
#'
#' @param DesignParam A list containing design parameters supplied from East or East Horizon.
#'        Common parameters include:
#'        \describe{
#'        \item{Alpha}{One-sided significance level}
#'        \item{TailType}{Tail direction, 1 = upper tail, 0 = lower tail}
#'        \item{NumTreatments}{Number of treatment arms excluding control}
#'        \item{MaxEvents}{Maximum number of events required for analysis}
#'        \item{CriticalPoint}{Critical boundary for fixed sample designs}
#'        \item{IsArmPresent}{Vector indicating which treatment arms remain active}
#'        }
#'
#' @param LookInfo Optional list containing interim analysis information.
#'        If supplied, common elements include:
#'        \describe{
#'        \item{NumLooks}{Total number of analyses}
#'        \item{CurrLookIndex}{Current analysis index}
#'        \item{InfoFrac}{Information fraction for each look}
#'        \item{EffBdry}{Efficacy boundaries for each look}
#'        }
#'
#' @param UserParam A list of user defined parameters in East or East Horizon. You must have a default of NULL, as in this example.
#'        If UserParam are supplied, they will be available as elements in the list UserParam.
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'         elements of the list, if the element is required or optional and a description of the return values if needed.
#'         \describe{
#'         \item{Decision}{Required integer vector containing decision for each treatment arm
#'                         \describe{
#'                         \item{0}{Continue trial}
#'                         \item{2}{Reject null hypothesis / efficacy success}
#'                         \item{3}{Futility at final analysis}
#'                         }
#'                         }
#'         \item{HR}{Required numeric vector containing observed hazard ratios for each treatment arm versus control}
#'         \item{AnalysisTime}{Required numeric value containing the calendar time of the current analysis}
#'         \item{ErrorCode}{Optional integer value
#'                         \describe{
#'                         \item{ErrorCode = 0}{No Error}
#'                         \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                         \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                         }
#'                         }
#'         }
#'
#' @description
#' This template analyzes time-to-event endpoints using the log-rank test and Cox proportional hazards model.
#' The function supports both fixed sample and group sequential survival designs.
#' Bonferroni multiplicity adjustment is applied across active treatment arms.
#'
#' The function signature must remain unchanged. However, additional user-defined logic
#' and parameters may be incorporated through the UserParam list if needed.
{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{

```
# Step 1 - Load required package ####
# The survival package is required for the Cox proportional hazards model
# and the log-rank test computations
require( survival )


# Step 2 - Retrieve design and interim analysis information ####
# If interim look information is supplied use the current look specific
# efficacy boundaries and event counts. Otherwise use the fixed sample settings
if( !is.null( LookInfo ) )
{
    
    # Example interim design setup
    # nQtyOfLooks             <- LookInfo$NumLooks
    # nLookIndex              <- LookInfo$CurrLookIndex
    # vEfficacyBoundary       <- LookInfo$EffBdry[ nLookIndex ]
    
}
else
{
    
    # Example fixed sample setup
    # nQtyOfLooks             <- 1
    # nLookIndex              <- 1
    # vEfficacyBoundaryPScale <- DesignParam$Alpha
    
}


# Step 3 - Prepare the analysis dataset ####
# Create event times, determine the analysis cutoff time,
# censor subjects appropriately and compute observed follow-up times


# Step 4 - Initialize analysis variables ####
# Initialize vectors to store p-values, hazard ratios and decisions
# for each treatment arm


# Step 5 - Loop over treatment arms and compute statistics ####
# For each active treatment arm:
#   1. Subset treatment and control patients
#   2. Fit Cox proportional hazards model
#   3. Compute hazard ratio
#   4. Perform log-rank test
#   5. Store p-values and hazard ratios


# Step 6 - Apply multiplicity adjustment ####
# Apply Bonferroni adjustment to the raw p-values using the
# number of active treatment arms


# Step 7 - Make efficacy and futility decisions ####
# Compare adjusted p-values against the efficacy boundary.
# If efficacy is not reached at the final analysis then assign
# futility for that treatment arm


# Step 8 - Error checking ####
# Add any required validation checks and update the error code if needed
nError <- 0


# Step 9 - Build the return object ####
lReturn <- list(
    Decision    = as.integer( vDecision ),
    ErrorCode   = as.integer( nError ),
    HR          = as.double( vHRRatio ),
    AnalysisTime = as.double( dTimeOfAnalysis )
)

return( lReturn )
```

}
    
    # Option 2: Script returns adjusted p value ####
    # Use this option if you want to calculate the adjusted p value with your own logic
    # but want to use the Decision generation logic of East Horizon
    vAdjPVal <- 0
    vHR <- 0
    # Setup adjusted p value calculation logic
    return( list(AdjPVal = vAdjPVal,
                 HR = vHR,
                 ErrorCode = as.integer(nError)) )
    
    # Option 3: Script returns raw p value ####
    # Use this option if you want to calculate the raw p values with your own logic
    # but want to use the Decision generation logic of East Horizon
    vRawPVal <- 0
    vHR <- 0
    # Setup raw p value calculation logic
    return( list(RawPVal = vRawPVal,
                 HR = vHR,
                 ErrorCode = as.integer(nError)) )

    # Option 4: Script returns test statistic ####
    # Use this option if you want to calculate the test statistic with your own logic
    # but want to use the Decision generation logic of East Horizon
    vTestStat <- 0
    vHR <- 0
    # Setup test statistic calculation logic
    return( list(TestStat = vTestStat,
                 HR = vHR,
                 ErrorCode = as.integer(nError)))
    
}

