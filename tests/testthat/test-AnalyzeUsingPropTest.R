#################################################################################################### .
#   Program/Function Name:
#   Author:Audrey Wathen
#   Description: Test file for AnalyzeUsingPropTest
#   Change History:
#   Last Modified Date: 06/20/2024
#################################################################################################### .


context( "AnalyzeUsingPropTest")

test_that("Test- AnalyzeUsingPropTest", {
    nQtyOfPatientsPerArm <- 125
    nQtyOfPatients       <- 2*nQtyOfPatientsPerArm
    
    vTreatmentID        <- c( rep(0,nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm) )
    vPatientResponseStd <- rbinom( nQtyOfPatientsPerArm,1, 0.25 )
    vPatientResponseExp <- rbinom( nQtyOfPatientsPerArm,1, 0.5 )
    vPatientResponse    <- c( vPatientResponseStd, vPatientResponseExp )
    
    vRandomIndex        <- sample( 1:nQtyOfPatients,  size = nQtyOfPatients )
    
    vPatientResponse    <- vPatientResponse[ vRandomIndex ]
    vTreatmentID        <- vTreatmentID[ vRandomIndex ]
    
    SimData     <- list( TreatmentID = vTreatmentID, Response = vPatientResponse)
    LookInfo    <- list( NumLooks = 3, CurrLooKIndex = 1, CumCompleters = c( nQtyOfPatients/2, nQtyOfPatients ))
    DesignParam <- list( SampleSize = nQtyOfPatients, MaxCompleters = nQtyOfPatients)
    
    #Test 1  LookInfo = NULL, UserParam = NULL ####
    
    lRet1 <- tryCatch({
        AnalyzeUsingPropTest( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    }, error = function(e) {
        NULL
    })
    
    lExpRet1 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet1, lExpRet1, info = "Test 1: Return list did not match")
    
    #Test 2  UserParam = NULL####
    
    lRet2 <- tryCatch({
        AnalyzeUsingPropTest( SimData, DesignParam, LookInfo = LookInfo, UserParam = NULL )
    }, error = function(e) {
        NULL
    })
    
    lExpRet2 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet2, lExpRet2, info = "Test 2: Return list did not match")
    
    #Test 3 omit LookInfo, UserParam ####
    
    lRet3 <- tryCatch({
        AnalyzeUsingPropTest( SimData, DesignParam )
    }, error = function(e) {
        NULL
    })
    
    lExpRet3 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet3, lExpRet3, info = "Test 3: Return list did not match")
    
})



