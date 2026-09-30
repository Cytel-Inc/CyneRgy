# Version 1
# Parameter Description 
# NumSub - The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
# NumArm - The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
# The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
# SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
# NumPrd - Number of time periods that are provided.  [QUESTION: Is this just the number of rows in SurvParam]
# PrdTime: 
#      If SurvMethod = 1: PrdTime is a vector of starting times of hazard pieces.
#      If SurvMethod = 2: Times at which the cumulative % survivals are specified.
#      If SurvMethod = 3: Period time is 0 by default
# SurvParam - Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
# If SurvMethod is 1:
#   SurvParam is an array (NumPrd rows,NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#   Arms are in columns with column 1 is control, column 2 is experimental
#   Time periods are in rows, row 1 is time period 1, row 2 is time period 2...
# If SurvMethod is 2:
#   SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.
# If SurvMethod is 3:
#   SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental 
# Description: The if/else structure provides the possible options for SurvMethod with an error defined at each one.  It is a good practice to specify all 3 options if possible.  H
#               However, if this cannot be done then which ever options are not provided should cause an error so it is clear that something was not as expected. 
SimulatePatientSurvivalExp <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam) 
{
    # The SurvParam depends on input in East. EAST sends the table found in the Simulation->Response Generation tab 
    if(SurvMethod == 1)   # Hazard Rates
    {
        ErrorCode <- ERROR1
            
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {

        ErrorCode <- ERROR2
       
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3

    }
   
    return(list(SurvivalTime = as.double(vSurvTime), ErrorCode = ErrorCode) )
}


