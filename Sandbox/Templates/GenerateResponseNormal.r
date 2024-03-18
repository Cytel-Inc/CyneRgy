# Function Template for Generating Response Values for Single Mean Test
GenRespSingleMean <- function(NumSub, Mean, StdDev)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 0	
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}

	# Write the actual code here.
	# Store the generated response values in an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 
	
	return(list(Response = as.double(retval), ErrorCode = as.integer(Error)))
}

#  Function Template for Generating Response Values for Difference of Means Test
GenRespDiffOfMeans <- function(NumSub,TreatmentID, Mean, StdDev)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 0	
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}
	
	# Write the actual code here.
	# Store the generated continuous response values in # an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 
        
	return(list(Response = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Response Values for Mean of Paired Differences Test
# Format1
GenRespPairedDiffFormat1 <- function(NumSub, Mean, SigmaD)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 0	
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}
	
	# Write the actual code here.
	# Store the generated difference of response 
	# values in an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 
        
	return(list(DiffResp = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Response Values for Mean of Paired Differences Test
# Format2
GenRespPairedDiffFormat2  <- function(NumSub, Mean, SigmaD)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval1	= c()
	retval2	= c()

	# Initialising Response Arrays to 0	
    for(i in 1:NumSub)
	{
	  retval1[i] = 0;	
	  retval2[i] = 0;	  
	}

	# Write the actual code here.
	# Store the generated Responses on Control Arm 
	# in an array called retval1
	# Store the generated Responses on Treatment Arm 
	# in an array called retval2
	# Use appropriate error handling and modify the
	# Error appropriately 
        
	return(list(RespC = as.double(retval1), RespT = as.double(retval2), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Response Values for Mean of Paired Ratio Test
# Format 1
GenRespPairedRatioFormat1 <- function(NumSub, Mean, StdDevLogRatio)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 1
    for(i in 1:NumSub)
	{
	  retval[i] = 1;	
	}

	# Write the actual code here.
	# Store the generated Ratio of response 
	#response values in an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 
        
	return(list(RatioResp = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Response Values for Mean of Paired Ratio Test
# Format 2
GenRespPairedRatioFormat2 <- function(NumSub, Mean, StdDevLogRatio)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval1	= c()
	retval2	= c()

	# Initialising Response Arrays to 1	
    for(i in 1:NumSub)
	{
	  retval1[i] = 1;	
	  retval2[i] = 1;	  
	}

	# Write the actual code here.
	# Store the generated Responses on Control Arm 
	# in an array called retval1
	# Store the generated Responses on Treatment Arm 
	# in an array called retval2
	# Use appropriate error handling and modify the
	# Error appropriately 
        
	return(list(RespC = as.double(retval1), RespT = as.double(retval2), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Response Values for Ratio of Means Test
GenRespRatioOfMeans <- function(NumSub,TreatmentID, Mean, CV)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()

	# Initialising Response Array to 1	
    for(i in 1:NumSub)
	{
	  retval[i] = 1;	
	}

	# Write the actual code here.
	# Store the generated response values in 
	# an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(Response = as.double(retval), ErrorCode = as.integer(Error)))
}

