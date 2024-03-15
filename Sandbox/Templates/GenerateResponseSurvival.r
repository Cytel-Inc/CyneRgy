# Function Template for Generating Survival Times (Time to Response)
GenSurvTime <- function(NumSub, NumArm, TreatmentID,  SurvMethod, NumPrd, PrdTime, SurvParam)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 0	
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}

	if(SurvMethod == 1)   # Hazard Rates
	{
		# Write the actual code for SurvMethod 1 
		# here.
		# Store the generated survival times in an 
		# array called retval.
	}

	if(SurvMethod == 2)   # Cumulative % Survivals
	{
		# Write the actual code for SurvMethod 2 
		# here.
		# Store the generated survival times in an 
		# array called retval.
	}
	
		if(SurvMethod == 3)   # Median Survival Times
	{
		# Write the actual code for SurvMethod 3 
		# here.
		# Store the generated survival times in an 
		# array called retval.
	}

	# Use appropriate error handling and modify the
	# Error appropriately in each of the methods

	return(list(SurvivalTime = as.double(retval), ErrorCode = as.integer(Error)))
}
