

MMRMAna <-function(SimData, DesignParam, UserParam = NULL )
{
  Error <- 0
  primdeltaest = 0
  secdeltaest = 0
  stderror  = 1

  #SimData <- read.csv("C:\\Users\\shubham.lahoti\\Downloads\\MMRM codes and data\\MMRMSimData.csv")
  SimData$id <- 1:DesignParam$SampleSize
  NumVisit <- DesignParam$NumVisit

longData <- reshape(SimData, varying = paste0("Response", 1:NumVisit),
                    direction="long", sep="", idvar="id")
longData <- longData[order(longData$TreatmentID,longData$id,longData$time),]

y <- longData[longData$time != 1,]$Response 
y0 <- longData[longData$time == 1,]$Response 
y0 <- rep(y0, each = NumVisit - 1)

trt <- longData[longData$time != 5,]$TreatmentID

mmrm <- nlme::gls(y~y0+trt,
                  na.action=na.omit, data=longData,
                  correlation=nlme::corSymm(form=~time | id),
                  weights=nlme::varIdent(form=~1|time))
# mmrm <- gls(y~y0*factor(t)+TreatmentID*factor(t),
#             na.action=na.omit, data=longData,
#             correlation=nlme::corSymm(form=~time | id),
#             weights=nlme::varIdent(form=~1|time))
summary(mmrm)


  if(summary(mmrm)$tTable["trt","p-value"] <= 0.025)
    Decision <- 2
  else
    Decision <- 0
  retval <- summary(mmrm)$tTable["trt","t-value"]
  
  return(list(TestStat = as.double(retval), PrimDelta = as.double(primdeltaest), SecDelta = as.double(secdeltaest), ErrorCode = as.integer(Error)))

  #return(list(Decision = as.integer(Decision), ErrorCode = as.integer(Error), tStat = as.double(summary(mmrm)$tTable["trt","t-value"]), PVal = as.double(summary(mmrm)$tTable["trt","p-value"])))
}


