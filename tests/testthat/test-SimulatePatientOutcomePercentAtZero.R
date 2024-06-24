#################################################################################################### .
#   Program/Function Name:
#   Author: Audrey Wathen
#   Description: Test file for SimulatePatientOutcomePercentAtZero
#   Change History:
#   Last Modified Date: 6/20/2024
#################################################################################################### .


context( "SimulatePatientOutcomePercentAtZero")

test_that("Test- SimulatePatientOutcomePercentAtZero", {
    nQtyOfPatientsPerArm <- 125
    NumSub               <- 2*nQtyOfPatientsPerArm
    
    TreatmentID          <- c( rep(0,nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm) )
    Mean                 <- c(0,0)
    StdDev               <- c(1,1)
    
    #Test 1  UserParam = NULL ####
    
    lRet1 <- tryCatch({
        SimulatePatientOutcomePercentAtZero(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
    }, error = function(e) {
        NULL
    })
    
    lExpRet1 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet1, lExpRet1, info = "Test 1: Return list did not match")
    
    #Test 2  UserParam is defined####
    
    UserParam     <-list( dProbOfZeroOutcomeCtrl = 0.1, dProbOfZeroOutcomeExp = 0.1 )
    
    lRet2 <- tryCatch({
        SimulatePatientOutcomePercentAtZero(NumSub, TreatmentID, Mean, StdDev, UserParam )
    }, error = function(e) {
        NULL
    })
    
    lExpRet2 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet2, lExpRet2, info = "Test 2: Return list did not match")
    
    #Test 3 omit UserParam ####
    
    lRet3 <- tryCatch({
        SimulatePatientOutcomePercentAtZero(NumSub, TreatmentID, Mean, StdDev)
    }, error = function(e) {
        NULL
    })
    
    lExpRet3 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet3, lExpRet3, info = "Test 3: Return list did not match")
    
})



