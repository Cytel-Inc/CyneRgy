setwd("~/R/CyneRgy_3/inst/Examples/MultiArmTwoEndpointTwoStageTrial/R")
source("SimulateBinaryAndPFS.R")

#------------under H0-------------
NumSub = 600
NumArm = 3
ArrivalTime = seq(1, NumSub, 1)
TreatmentID = rep(c(0,1,2), 200)
PropResp = c(0.1, 0.9, 0.1) # Arm 2 wins
UserParam = list(MedianSurvCtrl = 20,
                 HR1 = 0.7,
                 HR2 = 0.8)

PatientData <- SimulateBinaryAndPFS (NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL)

#####
SimData = data.frame(Response = PatientData$Response,
                     PFSNonCens = PatientData$PFSNonCens,
                     ArrivalTime = ArrivalTime,
                     TreatmentID = TreatmentID)

DesignParam = list(NumTreatments = 2,
                   CriticalPoint = 1.96,
                   Alpha = 0.025)  
LookInfo = NULL
UserParam = list(Stage1NumCompleters = 300,
                 Stage1FutThreshold = 0.1,
                 Stage1FutilityThreshold = 0.1,
                 DropoutProportion = 0,
                 TargetNumPFSEvents = 300,
                 SwitchSign = "yes")

SelectArmAndAnalyzePFSTwoStages ( SimData, DesignParam, LookInfo, UserParam  )
