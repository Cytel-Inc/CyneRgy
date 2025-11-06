#source("R/InsertPatientCSV.R")
source( "GeneratePatientFromCSV.R")

NumSub<- 200
NumVisit <- 5 
TreatmentID<- sample( 0:1,NumSub, replace=TRUE)
    
    Inputmethod <- NA 

VisitTime<- NA
MeanControl <- NA 
MeanTrt <- NA
StdDevControl <- NAStdDevTrt <- NA
CorrMat <- NA
UserParam <- list( InputFileName = "SimPatientDataAlt.csv")

rm( "gdfPatients")
lPat <- GeneratePatientFromCSV(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime,
                 MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat,
                 UserParam )

mean( lPat$Response5[ TreatmentID == 0 ])

mean( lPat$Response5[ TreatmentID == 1 ])