# Function Template for computing Test Statistic for One Look Tests
ComputeSingleLookTestStat <- function(SimData, DesignParam)
{
    # TO DO : Modify this function apprpriately
	
	Error 	= 0
	retval 	= 0
	
	# Write the actual code here.
	# Store the computed test statistic value in retval
	# Use appropriate error handling and modify the
	# Error appropriately

	return(list(TestStat = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for computing Test Statistic for Multi Look Tests
ComputeMultiLookTestStat <- function(SimData, DesignParam, LookInfo)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= 0
	deltaest = 0
	stderror = 1

	# Write the actual code here.
	# Store the computed test statistic value in retval
	# This test statistic value will be for particular look for # each simulation
	# Use appropriate error handling and modify the
	# Error appropriately

	return(list(TestStat = as.double(retval), Delta = as.double(deltaest), StdError = as.double(stderror), ErrorCode = as.integer(Error)))
}

# Function Template for computing Test Statistic for One Look Tests (Basic Simulation)
ComputeSingleLookBasicTestStat <- function(DesignParam)
{
	# TO DO : Modify this function apprpriately
	
	Error 	= 0
	retval 	= 0

	# Write the actual code here.
	# Store the computed test statistic value in retval
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(TestStat = as.double(retval), ErrorCode = as.integer(Error)))
}

# Function Template for computing Test Statistic for Multi Look Tests (Basic Simulations)
ComputeMultiLookBasicTestStat <- function(DesignParam, LookInfo)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= 0

	# Write the actual code here.
	# Store the computed test statistic value in retval
	# This test statistic value will be for particular look for # each simulation
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(TestStat = as.double(retval), ErrorCode = as.integer(Error)))
}


# Function Template for Performing Test for One Look Tests (Basic Simulations)
PerformSingleLookBasicDecision <- function(DesignParam)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= 0

	# Write the actual code here.
	# Compute test statistic value and store the decision
	# value (appropriate code) in retval
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(Decision = as.integer(retval), ErrorCode = as.integer(Error)))
}


# Function Template for Performing Test for Multi Look Tests
PerformMultiLookDecision <- function(SimData, DesignParam, LookInfo)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= 0
	
	# Write the actual code here.
	# Compute test statistic value and store the decision
	# value (appropriate code) in retval
	# Use appropriate error handling and modify the
	# Error appropriately

	return(list(Decision = as.integer(retval), ErrorCode = as.integer(Error)))
}

# Function Template for performing Test for One Look Tests
PerformSingleLookDecision <- function(SimData, DesignParam)
{
	# TO DO : Modify this function apprpriately

	Error 	= 0
	retval 	= 0

	# Write the actual code here.
	# Compute test statistic value and store the decision
	# value (appropriate code) in retval
	# Use appropriate error handling and modify the
	# Error appropriately 

	return(list(Decision = as.integer(retval), ErrorCode = as.integer(Error)))
}
