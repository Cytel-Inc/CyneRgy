#################################################################################################### .
#   Program/Function Name:
#   Author: Audrey Wathen
#   Description: Test file for SimulatePatientSurvivalWeibull
#   Change History:
#   Last Modified Date: 6/20/2024
#################################################################################################### .


context( "SimulatePatientSurvivalWeibull")

test_that("Test- SimulatePatientSurvivalWeibull", {
    nQtyOfPatientsPerArm <- 125
    NumSub               <- 2*nQtyOfPatientsPerArm
    NumArm               <-2
    SurvParam            <-1
    NumPrd               <-1
    PrdTime             <-1
    
    #TODO(kyle wathen) look up parameters for simulating survival data 
    
    TreatmentID          <- c( rep(0,nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm) )
    
    
    #Test 1  UserParam = NULL ####
    
    lRet1 <- tryCatch({
        SimulatePatientSurvivalWeibull(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL)
    }, error = function(e) {
        NULL
    })
    
    lExpRet1 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet1, lExpRet1, info = "Test 1: Return list did not match")
    
    #Test 2  UserParam is defined####
    
    UserParam     <-list( dShapeCtrl = 0.1, dScaleCtrl = 0.1, dShapeExp = 0.1, dScaleExp = 0.1 )
    
    lRet2 <- tryCatch({
        SimulatePatientSurvivalWeibull(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam)
    }, error = function(e) {
        NULL
    })
    
    lExpRet2 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet2, lExpRet2, info = "Test 2: Return list did not match")
    
    #Test 3 omit UserParam ####
    
    lRet3 <- tryCatch({
        SimulatePatientSurvivalWeibull(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam)
    }, error = function(e) {
        NULL
    })
    
    lExpRet3 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet3, lExpRet3, info = "Test 3: Return list did not match")
    
})