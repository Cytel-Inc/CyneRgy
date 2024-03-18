# Sample functions for geneartion of Arrival Times #######################################

###### GenArrivalTimeEqui Starts #######################################
# This function generates equidistant arrival times within each period
GenArrivalTimeEqui <- function(NumSub, NumPrd, PrdStart, AccrRate)
{
   options(warn = -1)
   Error <- 0
   retval <- c()
   maxAccrl <- c()
   for(i in 1:NumPrd){
   if(i!=NumPrd) {
    maxAccrl[i]  <- floor(AccrRate[i]*(PrdStart[i+1]-PrdStart[i]))
    for(j in 1:maxAccrl[i])
    retval <- c(retval, PrdStart[i]+ (PrdStart[i+1]-PrdStart[i])/(maxAccrl[i])*j)
    if(length(retval) == NumSub)
    break
    }else{
    maxAccrl[NumPrd] <- NumSub-sum(maxAccrl[1:(NumPrd-1)])
    x <- PrdStart[NumPrd] + maxAccrl[NumPrd]/AccrRate[NumPrd]
    for(j in 1:maxAccrl[NumPrd])
    retval <- c(retval, PrdStart[NumPrd]+ (x-PrdStart[NumPrd])/(maxAccrl[NumPrd])*j)
     if(length(retval) == NumSub)
     break
    }}
    if(any(is.na(retval)==TRUE) || length(retval)!= NumSub)
    Error <- -1
    return(list(ArrivalTime = as.double(retval), ErrorCode=as.integer(Error)))
}
###### GenArrivalTimeEqui Ends ##########################################

##### GenArrivalTimeUni Starts ##########################################
# This function genrates uniform arrival times within each period
GenArrivalTimeUni <- function(NumSub, NumPrd, PrdStart, AccrRate)
{
   options(warn = -1)
   Error <- 0
   retval <- c()
   genTime <- c()
   maxAccrl <- c()
   for( i in 1:NumPrd)  {
   if(i!=NumPrd) {
    maxAccrl[i]  <- floor(AccrRate[i]*(PrdStart[i+1]-PrdStart[i]))
    genTime <- runif(maxAccrl[i], PrdStart[i], PrdStart[i+1])
    retval <- c(retval, genTime)
    if(length(retval) > NumSub)
    break
    } else {
    maxAccrl[NumPrd] <- NumSub-sum(maxAccrl[1:(NumPrd-1)])
    x <- PrdStart[NumPrd] + maxAccrl[NumPrd]/AccrRate[NumPrd]
    genTime <- runif(maxAccrl[NumPrd], PrdStart[NumPrd], x)
    retval <- c(retval, genTime)
    if(length(retval) > NumSub)
    break
    } }
    if(any(is.na(retval)==TRUE) || length(retval)!= NumSub)
    Error <- -1
    return(list(ArrivalTime = as.double(retval), ErrorCode=as.integer(Error)))
}
##### GenArrivalTimeUni Ends ##########################################