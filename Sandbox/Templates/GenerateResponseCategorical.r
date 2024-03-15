# Function Template for Generating Categorical Response Values
GenCatResp <- function(NumSub, NumGrp, GroupID, NumCat, PropResp)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()
	   
	# Initialising Response Array to 0
	# This effectively means that all the patients belong to the 0th category 
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}
	
	# Write the actual code here.
	# Store the generated multinomial response values 
	# in an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(CatID = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for Generating Binary Response Values
GenBinResp <- function(NumSub, NumArm, TreatmentID, PropResp)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= c()
 
	# Initialising Response Array to 0
	# This effectively means that all the patients have responded as failures 
    for(i in 1:NumSub)
	{
	  retval[i] = 0;	
	}
	
	# Write the actual code here.
	# Store the generated binary response values 
	# in an array called retval.
	# Use appropriate error handling and modify the
	# Error appropriately
	
	return(list(Response = as.double(retval), ErrorCode = as.integer(Error)))
}
